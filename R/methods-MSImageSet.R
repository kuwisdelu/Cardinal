
setMethod("initialize", "MSImageSet",
	function(.Object,
			imageData = SImageData(),
			pixelData = annotatedDataFrameFrom(imageData),
			featureData = AnnotatedDataFrame(data.frame(mz=double())),
			processingData = new("MSImageProcess"),
			protocolData = AnnotatedDataFrame(),
			experimentData = new("MIAPE-Imaging"),
			...) {
		colnames(spectra(imageData)) <- pixelNames(pixelData)
		rownames(spectra(imageData)) <- featureNames(featureData)
		callNextMethod(.Object,
			imageData=imageData,
			pixelData=pixelData,
			featureData=featureData,
			processingData=processingData,
			protocolData=protocolData,
			experimentData=experimentData,
			...)
	})

MSImageSet <- function(spectra, mz, coord,
	imageData = SImageData(data=spectra, coord=coord),
	pixelData = IAnnotatedDataFrame(data=coord,
		varMetadata=data.frame(labelType=rep("spatial", ncol(coord)))),
	featureData = AnnotatedDataFrame(data=data.frame(mz=mz)),
	processingData = new("MSImageProcess"),
	protocolData = AnnotatedDataFrame(data.frame(
		row.names=sampleNames(pixelData))),
	experimentData = new("MIAPE-Imaging"),
	...)
{
	if ( missing(spectra) )
		imageData <- SImageData()
	if ( missing(mz) && missing(spectra) ) {
		mz <- double()
	} else if ( missing(mz) ) {
		mz <- seq_len(dim(spectra)[1])
	}
	if ( missing(coord) && missing(spectra) ) {
		pixelData <- annotatedDataFrameFrom(imageData)
	} else if ( missing(coord) || length(dim(spectra)) > 2 ) {
		grid <- lapply(dim(spectra)[-1], function(dm) 1:dm)
		coord <- expand.grid(grid)
		names(coord) <- names(dim(imageData@positionArray))
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
	dims <- dims(object)
	if ( ncol(dims) > 0 ) {
		if ( storageMode(object@imageData) != "immutableEnvironment" )
			msg <- validMsg(msg, "storageMode must be 'immutableEnvironment' for an MSImageSet")
		if ( is.null(object@featureData[["mz"]]) )
			msg <- validMsg(msg, "required column 'mz' missing from featureData")
		if ( nrow(spectra(object@imageData)) != nrow(object@featureData) )
			msg <- validMsg(msg, "number of features differ between imageData and featureData")
		if ( ncol(spectra(object@imageData)) != nrow(object@pixelData) )
			msg <- validMsg(msg, "number of pixels differ between imageData and pixelData")
		if ( any(rownames(spectra(object@imageData)) != featureNames(object@featureData)) )
			msg <- validMsg(msg, "feature names differ between imageData and featureData")
		if ( any(colnames(spectra(object@imageData)) != pixelNames(object@pixelData)) )
			msg <- validMsg(msg, "pixel names differ between imageData and pixelData")
		if ( !isTRUE(all.equal(object@imageData@positionArray, generatePositionArray(coord(object)))) )
			warning("positions are out of sync; run 'object <- regeneratePositions(object)' to resync")
	}
	if (is.null(msg)) TRUE else msg	
})

setMethod("mz", "MSImageSet", function(object) object@featureData[["mz"]])
setReplaceMethod("mz", "MSImageSet",
	function(object, value) {
		object@featureData[["mz"]] <- value
		if ( validObject(object) )
			object
	})

setMethod("spectra", "MSImageSet", function(object) spectra(object@imageData))
setReplaceMethod("spectra", "MSImageSet",
	function(object, value) {
		spectra(object@imageData) <- value
		if ( validObject(object) )
			object
	})

setMethod("iData", "MSImageSet", function(object) spectra(object))
setReplaceMethod("iData", "MSImageSet",
	function(object, value) {
		spectra(object) <- value
		object
	})

setMethod("regeneratePositions", "MSImageSet",
	function(object) {
		object@imageData@positionArray <- generatePositionArray(coord(object))
		object
	})

setReplaceMethod("coord", "MSImageSet",
	function(object, value) {
		coord(object@pixelData) <- value
		object <- regeneratePositions(object)
		if ( validObject(object) )
			object
	})

setReplaceMethod("coordNames", "MSImageSet",
	function(object, value) {
		coordNames(object@pixelData) <- value
		object <- regeneratePositions(object)
		if ( validObject(object) )
			object
	})

setReplaceMethod("pixelNames", "MSImageSet",
	function(object, value) {
		pixelNames(object@pixelData) <- value
		colnames(spectra(object@imageData)) <- value
		if ( validObject(object) )
			object
	})

setReplaceMethod("featureNames", "MSImageSet",
	function(object, value) {
		featureNames(object@featureData) <- value
		rownames(spectra(object@imageData)) <- value
		if ( validObject(object) )
			object
	})

setMethod("[", "MSImageSet",
	function(x, i, j, ..., drop) {
		x@imageData[["spectra"]] <- x@imageData[["spectra"]][i,j,drop=FALSE]
		x@featureData <- x@featureData[i,,drop=FALSE]
		x@pixelData <- x@pixelData[j,,drop=FALSE]
		x <- regeneratePositions(x)
		if ( validObject(x) )
			x
	})






