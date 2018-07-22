#### Methods for MSProcessedImagingSpectraList ####
## ------------------------------------------------

MSProcessedImagingSpectraList <- function(data) {
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
	object <- as(data, "MSProcessedImagingSpectraList", strict=FALSE)
	if ( validObject(object) )
		object
}

.valid.MSProcessedImagingSpectraList <- function(object) {
	errors <- NULL
	data <- as(object, "SimpleList", strict=FALSE)
	classes <- sapply(data, function(x) class(x))
	if ( length(data) > 0 && !all(classes %in% "sparse_matc") )
		errors <- c(errors , "elements must be of class 'sparse_matc'")
	if ( is.null(errors) ) TRUE else errors
}

setValidity("MSProcessedImagingSpectraList",
	.valid.MSProcessedImagingSpectraList)

setMethod("[", "MSProcessedImagingSpectraList",
	function(x, i, j, ..., drop = NULL) {
		if ( is.logical(drop) )
			x <- .SimpleImageArrayList(x)
		.subsetSimpleImageArrayList(x, i, j, drop)
	})

setReplaceMethod("[[", "MSProcessedImagingSpectraList",
	function(x, i, j, ..., value) {
		if ( !inherits(value, "sparse_matc") )
			x <- .SimpleImageArrayList(x)
		callNextMethod(x, i=i, ..., value=value)
	})

# manipulate underlying sparse spectra data

setMethod("keys", "MSProcessedImagingSpectraList",
	function(object) keys(object[[1L]]))

setReplaceMethod("keys", "MSProcessedImagingSpectraList",
	function(object, value) {
		fun <- function(x) {
			keys(x) <- value
			x
		}
		data <- as(object, "SimpleList", strict=FALSE)
		as(endoapply(data, fun), class(object))
	})

setMethod("tolerance", "MSProcessedImagingSpectraList",
	function(object) tolerance(object[[1L]]))

setReplaceMethod("tolerance", "MSProcessedImagingSpectraList",
	function(object, value) {
		fun <- function(x) {
			tolerance(x) <- value
			x
		}
		data <- as(object, "SimpleList", strict=FALSE)
		as(endoapply(data, fun), class(object))
	})

setMethod("combiner", "MSProcessedImagingSpectraList",
	function(object) combiner(object[[1L]]))

setReplaceMethod("combiner", "MSProcessedImagingSpectraList",
	function(object, value) {
		fun <- function(x) {
			combiner(x) <- value
			x
		}
		data <- as(object, "SimpleList", strict=FALSE)
		as(endoapply(data, fun), class(object))
	})

