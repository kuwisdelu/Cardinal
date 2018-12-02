
#### Methods for ResultImagingExperiment ####
## ------------------------------------------

setMethod("resultData", c("ResultImagingExperiment", "missing"), 
	function(object, i, ...) object@resultData)

setMethod("resultData", c("ResultImagingExperiment", "ANY"), 
	function(object, i, ...) object@resultData[[i]])

setReplaceMethod("resultData", c("ResultImagingExperiment", "missing"),
	function(object, i, ..., value) {
		object@resultData <- value
		if ( validObject(object) )
			object
	})

setReplaceMethod("resultData", c("ResultImagingExperiment", "ANY"),
	function(object, i, ..., value) {
		object@resultData[[i]] <- value
		if ( validObject(object) )
			object
	})

setMethod("modelData", "ResultImagingExperiment",
	function(object) object@modelData)

setReplaceMethod("modelData", "ResultImagingExperiment",
	function(object, value) {
		object@modelData <- value
		if ( validObject(object) )
			object			
	})

# show

.show_ResultImagingExperiment <- function(object) {
	t1 <- "    "
	# resultData()
    if ( length(resultData(object)) > 0L )
		.scat("resultData(%d): %s\n", names(resultData(object)), prefix=t1)
	# modelData()
	.scat("modelData(%d): %s\n", names(modelData(object)), prefix=t1)
}

setMethod("show", "SparseResultImagingExperiment",
	function(object) {
		callNextMethod(object)
		.show_ResultImagingExperiment(object)
	}
)

# coerce from ResultSet

.coerce_ResultImagingExperiment <- function(from, toclass) {
	results <-lapply(resultData(from), as, "List")
	new(toclass,
		imageData=.SimpleImageArrayList(),
		featureData=XDataFrame(fData(from)),
		elementMetadata=PositionDataFrame(
			coord=DataFrame(coord(from)[,coordLabels(from)],
				row.names=NULL),
			run=pixelData(from)$sample),
		resultData=as(results, "List"),
		modelData=DataFrame(pData(modelData(from))))
}

