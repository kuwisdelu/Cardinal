#### Methods for MSContinuousImagingSpectraList ####
## ------------------------------------------------

MSContinuousImagingSpectraList <- function(data) {
	if ( !is(data, "SimpleList") ) {
		if ( is.list(data) || !is.null(dim(data)) ) {
			data <- SimpleList(data)
		} else {
			stop("'data' must be a SimpleList, list or matrix")
		}
	}
	if ( is.null(names(data)) ) {
		inames <- .format.numbered("intensity", length(data))
		names(data) <- inames
	}
	object <- as(data, "MSContinuousImagingSpectraList", strict=FALSE)
	if ( validObject(object) )
		object
}

.valid.MSContinuousImagingSpectraList <- function(object) {
	errors <- NULL
	data <- as(object, "SimpleList", strict=FALSE)
	classes <- sapply(data, function(x) class(x))
	if ( length(data) > 0 && !all(classes %in% c("matrix", "matter_matc")) )
		errors <- c(errors , "elements must be of class 'matrix' or 'matter_matc'")
	if ( is.null(errors) ) TRUE else errors
}

setValidity("MSContinuousImagingSpectraList",
	.valid.MSContinuousImagingSpectraList)

setMethod("[", "MSContinuousImagingSpectraList",
	function(x, i, j, ..., drop = NULL)
		.subsetSimpleImageArrayList(x, i, j, drop))
