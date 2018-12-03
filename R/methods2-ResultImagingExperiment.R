
#### Methods for ResultImagingExperiment ####
## ------------------------------------------

.valid.ResultImagingExperiment <- function(object) {
	errors <- NULL
	if ( length(object@resultData) != nrow(object@modelData) )
		errors <- c(errors , paste("length of 'resultData'",
			"must match number of rows in 'modelData'"))
	if ( is.null(errors) ) TRUE else errors
}

setValidity("ResultImagingExperiment", .valid.ResultImagingExperiment)

setMethod("resultData", c("ResultImagingExperiment", "missing"), 
	function(object, ...) object@resultData)

setMethod("resultData", c("ResultImagingExperiment", "ANY"), 
	function(object, i, j, ...) {
		if ( missing(j) ) {
			object@resultData[[i, exact=FALSE]]
		} else {
			object@resultData[[i, exact=FALSE]][[j, exact=FALSE]]
		}
	})

setMethod("resultNames", "ResultImagingExperiment", 
	function(object, i, ...) {
		if ( missing(i) ) {
			lapply(resultData(object), names)
		} else {
			names(resultData(object, i))
		}
	})

setMethod("modelData", "ResultImagingExperiment",
	function(object) object@modelData)

setReplaceMethod("modelData", "ResultImagingExperiment",
	function(object, value) {
		object@modelData <- value
		if ( validObject(object) )
			object			
	})

setMethod("[[", c("ResultImagingExperiment", "ANY", "missing"),
	function(x, i, j, ...) {
		lapply(x@resultData, function(res) res[[i]])
	})

setMethod("[[", c("ResultImagingExperiment", "ANY", "ANY"),
	function(x, i, j, ...) {
		x@resultData[[i]][[j]]
	})

.DollarNames.ResultImagingExperiment <- function(x, pattern = "")
	grep(pattern, names(resultData(x, 1L)), value=TRUE)

setMethod("$", "ResultImagingExperiment",
	function(x, name) {
		lapply(x@resultData, function(res) res[[name, exact=FALSE]])
	})

# show

.show.ResultImagingExperiment <- function(object) {
	t1 <- "    "
	# resultData()
	if ( !is.null(resultNames(object, 1L)) )
		.scat("resultNames(%d): %s\n", resultNames(object, 1L), prefix=t1)
	resultDataNames <- names(resultData(object))
	if ( is.null(resultDataNames) )
		resultDataNames <- character(length(resultData(object)))
	.scat("resultData(%d): %s\n", resultDataNames, prefix=t1)
	# modelData()
	.scat("modelData(%d): %s\n", names(modelData(object)), prefix=t1)
}

setMethod("show", "ResultImagingExperiment",
	function(object) {
		callNextMethod(object)
		.show.ResultImagingExperiment(object)
	}
)

setMethod("show", "SparseResultImagingExperiment",
	function(object) {
		.show_SIE <- selectMethod("show", "SparseImagingExperiment")
		.show_SIE(object)
		.show.ResultImagingExperiment(object)
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

