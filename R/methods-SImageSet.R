
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


setMethod("iData", "SImageSet", function(x, i, ...) iData(x@imageData))
setReplaceMethod("iData", "SImageSet",
	function(x, i, ..., value) {
		iData(x@imageData) <- value
		x
	})

setMethod("regeneratePositions", "SImageSet",
	function(object) {
		if ( !identical(coord(object), coord(imageData(object))) )
			coord(imageData(object)) <- coord(object)
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
		x@imageData <- x@imageData[i,j,drop=NULL]
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

setMethod("is3D", "SImageSet",
	function(object) is3D(pixelData(object)))

## Adapted from as(ExpressionSet, data.frame) from Biobase
setAs("SImageSet", "data.frame",
	function(from) data.frame(t(iData(from)), pData(from)))
