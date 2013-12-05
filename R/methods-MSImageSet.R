
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
	if ( is.null(object@featureData[["mz"]]) )
		msg <- validMsg(msg, "required column 'mz' missing from featureData")
	if (is.null(msg)) TRUE else msg	
})

setMethod("mz", "MSImageSet", function(object) object@featureData[["mz"]])
setReplaceMethod("mz", "MSImageSet",
	function(object, value) {
		object@featureData[["mz"]] <- value
		if ( validObject(object) )
			object
	})

setMethod("spectra", "MSImageSet", function(object) iData(object))
setReplaceMethod("spectra", "MSImageSet",
	function(object, value) {
		iData(object) <- value
		object
	})




