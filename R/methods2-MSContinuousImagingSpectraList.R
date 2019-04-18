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
	object <- .to_MSContinuousImagingSpectraList(data)
	if ( validObject(object) )
		object
}

.valid.MSContinuousImagingSpectraList <- function(object) {
	errors <- NULL
	data <- as(object, "SimpleList", strict=FALSE)
	classes_ok <- sapply(data, function(x) inherits(x, c("matrix", "matter_matc")))
	if ( length(data) > 0 && !all(classes_ok) )
		errors <- c(errors , "elements must be of class 'matrix' or 'matter_matc'")
	if ( is.null(errors) ) TRUE else errors
}

setValidity("MSContinuousImagingSpectraList",
	.valid.MSContinuousImagingSpectraList)

setMethod("[", "MSContinuousImagingSpectraList",
	function(x, i, j, ..., drop = NULL)
		.subsetSimpleImageArrayList(x, i, j, drop))

setReplaceMethod("[[", "MSContinuousImagingSpectraList",
	function(x, i, j, ..., value) {
		if ( !inherits(value, c("matrix", "matter_matc")) )
			x <- .SimpleImageArrayList(x)
		callNextMethod(x, i=i, ..., value=value)
	})

.to_MSContinuousImagingSpectraList <- function(from) {
	fun <- function(x) {
		if ( !inherits(x, c("matrix", "matter_matc")) ) {
			as.matrix(x)
		} else {
			x
		}
	}
	data <- as(from, "SimpleList", strict=FALSE)
	as(endoapply(data, fun), "MSContinuousImagingSpectraList")
}
