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
	object <- .to_MSProcessedImagingSpectraList(data)
	if ( validObject(object) )
		object
}

.valid.MSProcessedImagingSpectraList <- function(object) {
	errors <- NULL
	data <- as(object, "SimpleList", strict=FALSE)
	classes_ok <- sapply(data, function(x) inherits(x, "sparse_mat"))
	if ( length(data) > 0 && !all(classes_ok) )
		errors <- c(errors , "elements must be of class 'sparse_mat'")
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
		if ( !inherits(value, "sparse_mat") )
			x <- .SimpleImageArrayList(x)
		callNextMethod(x, i=i, ..., value=value)
	})

.to_MSProcessedImagingSpectraList <- function(from, mz) {
	fun <- function(x) {
		if ( !inherits(x, "sparse_mat") ) {
			tol <- c(absolute=min(abs(diff(mz))))
			x <- sparse_mat(data=x,
				index=matrix(mz, nrow=nrow(x), ncol=ncol(x)),
				nrow=nrow(x), ncol=ncol(x), domain=mz,
				pointers=seq(0, length(x), by=nrow(x)),
				tolerance=tol, sampler="none")
		}
		x
	}
	data <- as(from, "SimpleList", strict=FALSE)
	as(endoapply(data, fun), "MSProcessedImagingSpectraList")
}

# manipulate underlying sparse spectra data

setMethod("domain", "MSProcessedImagingSpectraList",
	function(x) domain(x[[1L]]))

setReplaceMethod("domain", "MSProcessedImagingSpectraList",
	function(x, value) {
		fun <- function(y) {
			domain(y) <- value
			y
		}
		data <- as(x, "SimpleList", strict=FALSE)
		as(endoapply(data, fun), class(x))
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

setMethod("sampler", "MSProcessedImagingSpectraList",
	function(object) sampler(object[[1L]]))

setReplaceMethod("sampler", "MSProcessedImagingSpectraList",
	function(object, value) {
		fun <- function(x) {
			sampler(x) <- value
			x
		}
		data <- as(object, "SimpleList", strict=FALSE)
		as(endoapply(data, fun), class(object))
	})

