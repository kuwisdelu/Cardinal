
setMethod("initialize", "SImageSet",
	function(.Object,
			imageData = SImageData(),
			pixelData = annotatedDataFrameFrom(imageData),
			featureData = AnnotatedDataFrame(),
			...) {
		.Object <- callNextMethod(.Object,
			imageData=imageData,
			pixelData=pixelData,
			featureData=featureData,
			...)
		dmn.img <- list(featureNames(.Object@imageData), pixelNames(.Object@imageData))
		dmn.df <- list(featureNames(.Object@featureData), pixelNames(.Object@pixelData))
		if ( !isTRUE(all.equal(dmn.img, dmn.df, check.attributes=FALSE)) ) {
			featureNames(.Object@imageData) <- featureNames(.Object@featureData)
			pixelNames(.Object@imageData) <- pixelNames(.Object@pixelData)
		}
		.Object
	})

SImageSet <- function(
	data = Hashmat(nrow=0, ncol=0),
	coord = expand.grid(
		x = seq_len(prod(dim(data)[-1])),
		y = seq_len(ifelse(prod(dim(data)[-1]) > 0, 1, 0))),
	imageData = SImageData(
		data=data,
		coord=coord),
	pixelData = IAnnotatedDataFrame(
		data=coord,
		varMetadata=data.frame(
			labelType=rep("dim", ncol(coord)))),
	featureData = AnnotatedDataFrame(
		data=data.frame(row.names=seq_len(nrow(data)))),
	protocolData = AnnotatedDataFrame(
		data=data.frame(row.names=sampleNames(pixelData))),
	experimentData = new("MIAPE-Imaging"),
	...)
{
	if ( length(dim(data)) > 2 ) {
		coord <- mapply(seq_len, dim(data)[-1], SIMPLIFY=FALSE, USE.NAMES=TRUE)
		if ( is.null(names(coord)) || any(nchar(names(coord)) == 0) ) {
			if ( length(coord) %in% c(2,3) ) {
				names(coord) <- c("x", "y", "z")[seq_along(coord)]
			} else {
				names(coord) <- paste("dim", seq_along(coord), sep="")
			}
		}
		coord <- do.call("expand.grid", coord)
	}
	.SImageSet(imageData=imageData,
		pixelData=pixelData,
		featureData=featureData,
		protocolData=protocolData,
		experimentData=experimentData,
		...)
}

setValidity("SImageSet", function(object) {
	msg <- validMsg(NULL, NULL)
	dims <- dims(object)
	if ( ncol(dims) > 0 ) {
		if ( storageMode(object@imageData) != "immutableEnvironment" )
			msg <- validMsg(msg, "storageMode must be 'immutableEnvironment' for an SImageSet")
		if ( nrow(iData(object@imageData)) != nrow(object@featureData) )
			msg <- validMsg(msg, "number of features differ between imageData and featureData")
		if ( ncol(iData(object@imageData)) != nrow(object@pixelData) )
			msg <- validMsg(msg, "number of pixels differ between imageData and pixelData")
		if ( any(featureNames(object@imageData) != featureNames(object@featureData)) )
			msg <- validMsg(msg, "feature names differ between imageData and featureData")
		if ( any(pixelNames(object@imageData) != pixelNames(object@pixelData)) )
			msg <- validMsg(msg, "pixel names differ between imageData and pixelData")
	}
	if (is.null(msg)) TRUE else msg	
})


setMethod("iData", "SImageSet", function(object) iData(object@imageData))
setReplaceMethod("iData", "SImageSet",
	function(object, value) {
		iData(object@imageData) <- value
		object
	})

setMethod("regeneratePositions", "SImageSet",
	function(object) {
		object@imageData <- regeneratePositions(object@imageData)
		object
	})

setReplaceMethod("coord", "SImageSet",
	function(object, value) {
		coord(object@pixelData) <- value
		coord(object@imageData) <- value
		object@imageData <- regeneratePositions(object@imageData)
		object
	})

setReplaceMethod("coordLabels", "SImageSet",
	function(object, value) {
		which <- names(coord(object@imageData)) %in% coordLabels(object@pixelData)
		names(coord(object@imageData))[which] <- value
		coordLabels(object@pixelData) <- value
		object@imageData <- regeneratePositions(object@imageData)
		object
	})

setReplaceMethod("pixelNames", "SImageSet",
	function(object, value) {
		pixelNames(object@pixelData) <- value
		pixelNames(object@imageData) <- value
		object
	})

setReplaceMethod("featureNames", "SImageSet",
	function(object, value) {
		featureNames(object@featureData) <- value
		featureNames(object@imageData) <- value
		object
	})

setMethod("[", "SImageSet",
	function(x, i, j, ..., drop) {
		if ( missing(i) ) i <- seq_len(nrow(featureData(x)))
		if ( missing(j) ) j <- seq_len(nrow(pixelData(x)))
		i <- features(x)[i]
		j <- pixels(x)[j]
		x@imageData <- x@imageData[i,j,drop=NA]
		x@featureData <- x@featureData[i,,drop=FALSE]
		x@pixelData <- x@pixelData[j,,drop=FALSE]
		x@protocolData <- x@protocolData[sampleNames(x@pixelData),,drop=FALSE]
		coord(x@imageData) <- coord(x)
		x <- regeneratePositions(x)
		x
	})

setMethod("combine", signature = c(x = "SImageSet", y = "SImageSet"),
	function(x, y, ...) {
		sx <- pixelData(x)[["sample"]]
		sy <- pixelData(y)[["sample"]]
		if ( !identical(sort(unique(sx)), sort(unique(sy))) ) {
			if ( varMetadata(x)["sample", "labelType"] != "dim" ) {
				coord(imageData(x))[["sample"]] <- pixelData(x)[["sample"]]
				varMetadata(x)["sample", "labelType"] <- "dim"
			}
			if ( varMetadata(y)["sample", "labelType"] != "dim" ) {
				coord(imageData(y))[["sample"]] <- pixelData(y)[["sample"]]
				varMetadata(y)["sample", "labelType"] <- "dim"
			}
		}
		callNextMethod(x, y, ...)
	})

## Adapted from as(ExpressionSet, data.frame) from Biobase
setAs("SImageSet", "data.frame",
	function(from) data.frame(t(iData(from)), pData(from)))

setMethod("pixelApply", "SImageSet",
	function(.object, .fun, ...,
			.pixel,
			.feature,
			.feature.groups,
			.pixel.dependencies,
			.simplify = TRUE,
			.use.names = TRUE,
			.verbose = FALSE) {
		# set up subset variables if not provided
		if ( !missing(.pixel) )
			.pixel <- tryCatch(eval(substitute(.pixel), envir=pData(.object),
				enclos=environment(.fun)), error=function(e) eval(.pixel))
		if ( missing(.pixel) || is.null(.pixel) )
			.pixel <- rep(TRUE, nrow(.object@pixelData))
		.pixel <- pixels(.object)[.pixel]
		if ( !missing(.feature) )
			.feature <- tryCatch(eval(substitute(.feature), envir=fData(.object),
				enclos=environment(.fun)), error=function(e) eval(.feature))
		if ( missing(.feature) || is.null(.feature) )
			.feature <- rep(TRUE, nrow(.object@featureData))
		.feature <- features(.object)[.feature]
		# set up grouping variables if not provided
		if ( !missing(.feature.groups) )
			.feature.groups <- tryCatch(eval(substitute(.feature.groups), envir=fData(.object),
				enclos=environment(.fun)), error=function(e) eval(.feature.groups))
		if ( missing(.feature.groups) || is.null(.feature.groups) )
			.feature.groups <- factor(integer(length(.feature)), labels="")
		.feature.groups <- as.factor(.feature.groups)
		if ( !length(.feature.groups) %in% c(length(.feature), nrow(.object)) )
			.stop("'.feature.groups' must have length equal to '.feature' or '.object' feature extent")
		if ( length(.feature) != length(.feature.groups) )
			.feature.groups <- .feature.groups[.feature]
		groups <- split(.feature, .feature.groups, drop=TRUE)
		# set up function environment
		parent <- environment(.fun)
		if ( is.null(parent) )
			parent <- emptyenv()
		env <- new.env(parent=parent)
		if ( length(fData(.object)) != 0 )
			multiassign(names(fData(.object)), fData(.object), envir=env)
		assign(".Object", .object, envir=env)
		environment(.fun) <- env
		# prepare and calculate result
		ans <- vector("list", length(.pixel))
		.message(progress="start", max=length(.pixel))
		for ( i in seq_along(.pixel) ) {
			ans[[i]] <- sapply(groups, function(j) {
				assign(".Index", .pixel[i], envir=env)
				.fun(iData(.object)[j, .pixel[i]], ...)
			}, USE.NAMES=.use.names)
			.message(progress="increment")
		}
		.message(progress="stop")
		# simplify result
		if ( .use.names ) names(ans) <- names(.pixel)
		if ( .simplify ) ans <- simplify2array(ans)
		if ( .simplify && length(dim(ans)) > 2 && any(dim(ans) == 1) ) {
			dmn <- dimnames(ans)
			rmInd <- which(dim(ans) == 1)
			dim(ans) <- dim(ans)[-rmInd]
			dimnames(ans) <- dmn[-rmInd]
		}
		if ( .simplify && length(dim(ans)) == 1 )
			ans <- as.matrix(ans)
		ans
	})

setMethod("featureApply", "SImageSet",
	function(.object, .fun, ...,
			.feature,
			.pixel,
			.pixel.groups,
			.feature.dependencies,
			.simplify = TRUE,
			.use.names = TRUE,
			.verbose = FALSE) {
		# set up subset variables if not provided
		if ( !missing(.feature) )
			.feature <- tryCatch(eval(substitute(.feature), envir=fData(.object),
				enclos=environment(.fun)), error=function(e) eval(.feature))
		if ( missing(.feature) || is.null(.feature) )
			.feature <- rep(TRUE, nrow(.object@featureData))
		.feature <- features(.object)[.feature]
		if ( !missing(.pixel) )
			.pixel <- tryCatch(eval(substitute(.pixel), envir=pData(.object),
				enclos=environment(.fun)), error=function(e) eval(.pixel))
		if ( missing(.pixel) || is.null(.pixel) )
			.pixel <- rep(TRUE, nrow(.object@pixelData))
		.pixel <- pixels(.object)[.pixel]
		# set up grouping variables if not provided
		if ( !missing(.pixel.groups) )
			.pixel.groups <- tryCatch(eval(substitute(.pixel.groups), envir=pData(.object),
				enclos=environment(.fun)), error=function(e) eval(.pixel.groups))
		if ( missing(.pixel.groups) || is.null(.pixel.groups) )
			.pixel.groups <- factor(integer(length(.pixel)), labels="")
		.pixel.groups <- as.factor(.pixel.groups)
		if ( !length(.pixel.groups) %in% c(length(.pixel), ncol(.object)) )
			.stop("'.pixel.groups' must have length equal to '.pixel' or '.object' pixel extent")
		if ( length(.pixel) != length(.pixel.groups) )
			.pixel.groups <- .pixel.groups[.pixel]
		groups <- split(.pixel, .pixel.groups, drop=TRUE)
		# set up function environment
		parent <- environment(.fun)
		if ( is.null(parent) )
			parent <- emptyenv()
		env <- new.env(parent=parent)
		if ( length(pData(.object)) != 0 )
			multiassign(names(pData(.object)), pData(.object), envir=env)
		assign(".Object", .object, envir=env)
		environment(.fun) <- env
		# prepare and calculate result
		ans <- vector("list", length(.feature))
		.message(progress="start", max=length(.feature))
		for ( i in seq_along(.feature) ) {
			ans[[i]] <- sapply(groups, function(j) {
				assign(".Index", .feature[i], env)
				.fun(iData(.object)[.feature[i], j], ...)
			}, USE.NAMES=.use.names)
			.message(progress="increment")
		}
		.message(progress="stop")
		# simplify result
		if ( .use.names ) names(ans) <- names(.feature)
		if ( .simplify ) ans <- simplify2array(ans)
		if ( .simplify && length(dim(ans)) > 2 && any(dim(ans) == 1) ) {
			dmn <- dimnames(ans)
			rmInd <- which(dim(ans) == 1)
			dim(ans) <- dim(ans)[-rmInd]
			dimnames(ans) <- dmn[-rmInd]
		}
		if ( .simplify && length(dim(ans)) == 1 )
			ans <- as.matrix(ans)
		ans
	})

setMethod("cvApply", "SImageSet",
	function(.x, .y, .fun, .fold = sample, ...) {
		# setup
		x <- .x
		y <- .y
		# get cv folds and method
		.fun <- match.fun(.fun)
		.fold <- tryCatch(eval(substitute(.fold), envir=pData(x),
			enclos=environment(.fun)), error=function(e) eval(.fold))
		# loop through folds
		result <- lapply(sort(unique(.fold)), function(k) {
			.message("!!---- Fold ", k, " ----!!")
			if ( length(dim(y)) == 2 ) {
				fit <- .fun(x[,k!=.fold], y[k!=.fold,,drop=FALSE], ...)
				predict(fit,
					newx=x[,k==.fold],
					newy=y[k==.fold,,drop=FALSE], ...)
			} else {
				fit <- .fun(x[,k!=.fold], y[k!=.fold], ...)
				predict(fit,
					newx=x[,k==.fold],
					newy=y[k==.fold], ...)
			}
		})
		names(result) <- sort(unique(.fold))
		model <- AnnotatedDataFrame(data=data.frame(
				fold=sort(unique(.fold))),
			varMetadata=data.frame(
				labelDescription=c(
					fold="Fold")))
		new("CrossValidated",
			pixelData=x@pixelData,
			featureData=x@featureData,
			experimentData=x@experimentData,
			protocolData=x@protocolData,
			resultData=result,
			modelData=model)
	})

