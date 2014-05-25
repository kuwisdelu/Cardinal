
setMethod("initialize", "MSImageSet",
	function(.Object,
			imageData = SImageData(),
			pixelData = annotatedDataFrameFrom(imageData),
			featureData = AnnotatedDataFrame(data.frame(mz=double())),
			processingData = new("MSImageProcess"),
			protocolData = AnnotatedDataFrame(),
			experimentData = new("MIAPE-Imaging"),
			...) {
		featureNames(featureData) <- .formatMZ(featureData[["mz"]])
		pixelNames(pixelData) <- .formatCoord(coord(pixelData))
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
	imageData = SImageData(
		data=spectra,
		coord=coord),
	pixelData = IAnnotatedDataFrame(
		data=coord,
		varMetadata=data.frame(labelType=rep("dim", ncol(coord)))),
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
	if (is.null(msg)) TRUE else msg	
})

setMethod("mz", "MSImageSet", function(object) object@featureData[["mz"]])
setReplaceMethod("mz", "MSImageSet",
	function(object, value) {
		object@featureData[["mz"]] <- value
		object
	})

setMethod("spectra", "MSImageSet", function(object) iData(object))
setReplaceMethod("spectra", "MSImageSet",
	function(object, value) {
		iData(object) <- value
		object
	})

setMethod("combine", signature = c(x = "MSImageSet", y = "MSImageSet"),
	function(x, y, ...) {
		if ( varMetadata(x)["sample", "labelType"] != "dim" )
			varMetadata(x)["sample", "labelType"] <- "dim"
		if ( varMetadata(y)["sample", "labelType"] != "dim" )
			varMetadata(y)["sample", "labelType"] <- "dim"
		pixelNames(x) <- .formatCoord(coord(x))
		pixelNames(y) <- .formatCoord(coord(y))
		x <- callNextMethod(x, y, ...)
		x@processingData <- combine(x@processingData,
			y@processingData)
		regeneratePositions(x)
	})

