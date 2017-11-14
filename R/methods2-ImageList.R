#### Methods for ImageList ####
## ------------------------------------

ImageList <- function(data) {
	if ( !is(data, "SimpleList") ) {
		if ( is.list(data) || !is.null(dim(data)) ) {
			data <- SimpleList(data)
		} else {
			stop("'data' must be a SimpleList, list or array-like")
		}
	}
	object <- as(data, "SimpleImageList")
	if ( validObject(object) )
		object
}

.valid.ImageList <- function(object) {
	errors <- NULL
	data <- as(object, "SimpleList", strict=FALSE)
	if ( !is(data, "SimpleList") )
		errors <- c(errors , "'ImageList' must be coercible to SimpleList")
	dims <- sapply(data, function(x) length(dim(x)))
	if ( length(data) > 0 && any(dims == 0) )
		errors <- c(errors , "elements must be array-like (non-NULL 'dim')")
	if ( length(data) > 0 && length(unique(dims)) > 1 )
		errors <- c(errors , "elements must have the same number of dimensions")
	if ( is.null(errors) ) TRUE else errors
}

setValidity("ImageList", .valid.ImageList)

.getSL_length <- selectMethod("length", "SimpleList")
setMethod("length", "ImageList", 
	function(x) {
		data <- as(x, "SimpleList")
		.getSL_length(data)
	})

.getSL_names <- selectMethod("names", "SimpleList")
setMethod("names", "ImageList", 
	function(x) {
		data <- as(x, "SimpleList")
		.getSL_names(data)
	})

.setSL_names <- selectMethod("names<-", "SimpleList")
setReplaceMethod("names", "ImageList", 
	function(x, value) {
		data <- as(x, "SimpleList")
		data <- .setSL_names(data, value=value)
		as(data, class(x))
	})

.getSL_elements <- selectMethod("[[", "SimpleList")
setMethod("[[", "ImageList",
	function(x, i, j, ...) {
		data <- as(x, "SimpleList")
		.getSL_elements(data, i=i, j=j, ...)
	})

.setSL_elements <- selectMethod("[[<-", "SimpleList")
setReplaceMethod("[[", "ImageList",
	function(x, i, j, ..., value) {
		data <- as(x, "SimpleList")
		data <- .setSL_elements(data, i=i, j=j, ..., value=value)
		x <- as(data, class(x))
		if ( validObject(x) )
			x
	})

setMethod("dim", "ImageList",
	function(x) {
		if ( length(x) == 0 ) {
			0L
		} else {
			dim(x[[1]])
		}
	})

setMethod("dims", "ImageList",
	function(object) {
		if ( length(object) == 0 )
			return(matrix(nrow=0, ncol=0))
		data <- as(object, "SimpleList")
		sapply(data, dim, USE.NAMES=TRUE)
	})


