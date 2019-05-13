
#### Methods for AnnotatedImagingExperiment ####
## ------------------------------------------

AnnotatedImagingExperiment <- function(imageData = AnnotatedImageList(),
	featureData = DataFrame(), phenoData = DataFrame(), metadata = list())
{
	if ( !is(imageData, "AnnotatedImageList") )
		imageData <- AnnotatedImageList(imageData)
	if ( length(imageData) != 0L ) {
		if ( missing(featureData) )
			featureData <- new("DataFrame", nrows=nrow(imageData))
		if ( missing(phenoData) )
			phenoData <- new("DataFrame", nrows=ncol(imageData),
				rownames=names(imageData))
	}
	.AnnotatedImagingExperiment(
		imageData=imageData,
		featureData=featureData,
		elementMetadata=phenoData,
		metadata=metadata)
}

.valid.AnnotatedImagingExperiment <- function(object) {
	errors <- NULL
	if ( length(object@imageData) != 0L ) {
		if ( nrow(object@imageData) != nrow(object@featureData) )
			errors <- c(errors , paste("number of rows in 'featureData'",
				"must match number of rows in 'imageData'"))
		if ( ncol(object@imageData) != nrow(object@elementMetadata) )
			errors <- c(errors , paste("number of rows in 'phenoData'",
				"must match number of columns in 'imageData'"))
	}
	if ( is.null(errors) ) TRUE else errors
}

setValidity("AnnotatedImagingExperiment", .valid.AnnotatedImagingExperiment)


setMethod("dims", "AnnotatedImagingExperiment",
	function(x) dims(imageData(x)))

