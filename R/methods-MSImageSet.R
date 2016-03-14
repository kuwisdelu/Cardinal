
setMethod("initialize", "MSImageSet",
	function(.Object,
			imageData = MSImageData(),
			pixelData = annotatedDataFrameFrom(imageData),
			featureData = AnnotatedDataFrame(data.frame(mz=double())),
			processingData = new("MSImageProcess"),
			protocolData = AnnotatedDataFrame(),
			experimentData = new("MIAPE-Imaging"),
			...) {
		featureNames(featureData) <- make.unique(.format.mz(featureData[["mz"]]))
		pixelNames(pixelData) <- make.unique(.format.data.frame(coord(pixelData)))
		callNextMethod(.Object,
			imageData=imageData,
			pixelData=pixelData,
			featureData=featureData,
			processingData=processingData,
			protocolData=protocolData,
			experimentData=experimentData,
			...)
	})

MSImageSet <- function(
	spectra = Hashmat(nrow=0, ncol=0),
	mz = seq_len(dim(spectra)[1]),
	coord = expand.grid(
		x = seq_len(prod(dim(spectra)[-1])),
		y = seq_len(ifelse(prod(dim(spectra)[-1]) > 0, 1, 0))),
	imageData = MSImageData(
		data=spectra,
		coord=coord),
	pixelData = IAnnotatedDataFrame(
		data=coord,
		varMetadata=data.frame(
			labelType=rep("dim", ncol(coord)))),
	featureData = AnnotatedDataFrame(
		data=data.frame(mz=mz)),
	processingData = new("MSImageProcess"),
	protocolData = AnnotatedDataFrame(
		data=data.frame(row.names=sampleNames(pixelData))),
	experimentData = new("MIAPE-Imaging"),
	...)
{
	if ( length(dim(spectra)) > 2 ) {
		coord <- mapply(seq_len, dim(spectra)[-1], SIMPLIFY=FALSE, USE.NAMES=TRUE)
		if ( is.null(names(coord)) || any(nchar(names(coord)) == 0) ) {
			if ( length(coord) %in% c(2,3) ) {
				names(coord) <- c("x", "y", "z")[seq_along(coord)]
			} else {
				names(coord) <- paste("dim", seq_along(coord), sep="")
			}
		}
		coord <- do.call("expand.grid", coord)
	}
	.MSImageSet(imageData=imageData,
		pixelData=pixelData,
		featureData=featureData,
		processingData=processingData,
		protocolData=protocolData,
		experimentData=experimentData,
		...)
}

setValidity("MSImageSet", function(object) {
	msg <- validMsg(NULL, NULL)
	if ( is.null(object@featureData[["mz"]]) )
		msg <- validMsg(msg, "required column 'mz' missing from featureData")
	mz <- object@featureData[["mz"]]
	if ( !(all(cummax(mz) == mz) || all(cummin(mz) == mz)) )
		msg <- validMsg(msg, "'mz' must be strictly increasing or decreasing")
	if (is.null(msg)) TRUE else msg	
})

setMethod("spectra", "MSImageSet", function(object, ...) iData(object@imageData))
setReplaceMethod("spectra", "MSImageSet",
	function(object, value) {
		iData(object@imageData) <- value
		object
	})

setMethod("peaks", "MSImageSet", function(object, ...) peakData(object@imageData))
setReplaceMethod("peaks", "MSImageSet",
	function(object, value) {
		peakData(object@imageData) <- value
		object
	})

setMethod("mz", "MSImageSet", function(object, ...) object@featureData[["mz"]])
setReplaceMethod("mz", "MSImageSet",
	function(object, value) {
		object@featureData[["mz"]] <- value
		featureNames(object) <- make.unique(.format.mz(value))
		object
	})

setMethod("features", "MSImageSet",
	function(object, ..., mz) {
		if ( missing(mz) ) {
			features <- callNextMethod(object, ...)
		} else {
			mz <- as.numeric(mz)
			features <- sapply(mz, function(mzi) {
				# bisection.seq(mz(object), function(x) x - mzi)
				which.min(abs(mz(object) - mzi))
			})
			names(features) <- featureNames(object)[features]
			if ( length(list(...)) > 0 ) {
				keep <- features %in% callNextMethod(object, ...)
				features <- features[keep]
			}
		}
		features
	})

setMethod("pixels", "MSImageSet",
	function(object, ..., coord) {
		if ( missing(coord) ) {
			pixels <- callNextMethod(object, ...)
		} else {
			coord <- as.data.frame(as.list(coord))
			pixels <- unlist(apply(coord, 1, function(xyz) {
				do.call("pixels", args=c(list(object), xyz))
			}))
			names(pixels) <- pixelNames(object)[pixels]
			if ( length(list(...)) > 0 ) {
				keep <- pixels %in% callNextMethod(object, ...)
				pixels <- pixels[keep]
			}
		}
		pixels
	})

setMethod("centroided", "MSImageSet", function(object) centroided(object@processingData))
setReplaceMethod("centroided", "MSImageSet",
	function(object, value) {
		centroided(object@processingData) <- value
		object
	})

setMethod("processingData", "MSImageSet", function(object) object@processingData)
setReplaceMethod("processingData", "MSImageSet",
	function(object, value) {
		object@processingData <- value
		object
	})

setMethod("combine", signature = c(x = "MSImageSet", y = "MSImageSet"),
	function(x, y, ..., tolerance = 1e-5) {
		if ( !isTRUE(all.equal(mz(x), mz(y), tolerance=tolerance)) ) {
			.stop("MSImageSet 'mz' must match")
		} else if ( !identical(mz(x), mz(y)) ) {
			mz(y) <- mz(x)
		}
		x <- callNextMethod(x, y, ...)
		x@processingData <- combine(x@processingData,
			y@processingData)
		pixelNames(x) <- .format.data.frame(coord(x))
		x
	})

