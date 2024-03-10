
## coerce from MSImageSet

.from_legacy_MSImagSet <- function(from)
{
	fData <- from@featureData
	pData <- from@pixelData
	fDataNames <- setdiff(names(fData), "mz")
	coordLabelTypes <- "dim"
	sampleLabelTypes <- "sample"
	isCoord <- pData@varMetadata[["labelType"]] %in% coordLabelTypes
	isCoord[names(pData@data) %in% sampleLabelTypes] <- FALSE
	coordLabels <- names(pData@data)[isCoord]
	pDataNames <- setdiff(names(pData@data), c(coordLabels, "sample"))
	iData <- from@imageData@data[["iData"]]
	MSImagingExperiment(imageData=iData,
		featureData=MassDataFrame(
			mz=fData@data[["mz"]],
			fData@data[,fDataNames,drop=FALSE]),
		pixelData=PositionDataFrame(
			coord=DataFrame(pData@data[isCoord], row.names=NULL),
			run=pData@data$sample,
			pData@data[,pDataNames,drop=FALSE]),
		centroided=from@processingData@centroided)
}

setAs("MSImageSet", "MSImagingExperiment", .from_legacy_MSImagSet)


# coerce from ResultSet

.from_legacy_ResultSet <- function(from, toclass)
{
	fData <- from@featureData
	pData <- from@pixelData
	coordLabelTypes <- "dim"
	sampleLabelTypes <- "sample"
	isCoord <- pData@varMetadata[["labelType"]] %in% coordLabelTypes
	isCoord[names(pData@data) %in% sampleLabelTypes] <- FALSE
	coordLabels <- names(pData@data)[isCoord]
	new(toclass,
		imageData=.SimpleImageArrayList(),
		featureData=XDataFrame(fData@data),
		elementMetadata=PositionDataFrame(
			coord=DataFrame(pData@data[coordLabels], row.names=NULL),
			run=pData@data$sample),
		resultData=as(from@resultData, "List"),
		modelData=DataFrame(from@modelData@data))
}


