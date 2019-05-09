#### Methods for ImageList ####
## -----------------------------

# Heavily borrows from Assays class from SummarizedExperiment
# but with fewer assumptions about the dimensions of the arrays
#
# ImageList (VIRTUAL class -- only expects non-NULL 'dim')
# > SimpleImageList (SimpleList implementation of ImageList)
# > > SimpleImageArrayList (enforces dimensionality contraints)

ImageList <- function(data) {
	if ( !is(data, "SimpleList") && !is(data, "ImageList") ) {
		if ( is.list(data) || !is.null(dim(data)) ) {
			data <- SimpleList(data)
		} else {
			stop("'data' must be a SimpleList, list or array-like")
		}
	}
	object <- as(data, "SimpleImageList", strict=FALSE)
	if ( validObject(object) )
		object
}

.valid.ImageList <- function(object) {
	errors <- NULL
	data <- as(object, "SimpleList", strict=FALSE)
	if ( !is(data, "SimpleList") )
		errors <- c(errors , "'ImageList' must be coercible to SimpleList")
	dimlengths <- sapply(data, function(x) length(dim(x)))
	if ( length(data) > 0 && any(dimlengths == 0) )
		errors <- c(errors , "elements must be array-like (non-NULL 'dim')")
	if ( is.null(errors) ) TRUE else errors
}

setValidity("ImageList", .valid.ImageList)

setMethod("as.list", "ImageList", function(x)
	as.list(as(x, "SimpleList")))

.getSL_length <- selectMethod("length", "SimpleList")
setMethod("length", "ImageList", 
	function(x) {
		data <- as(x, "SimpleList", strict=FALSE)
		.getSL_length(data)
	})

.getSL_names <- selectMethod("names", "SimpleList")
setMethod("names", "ImageList", 
	function(x) {
		data <- as(x, "SimpleList", strict=FALSE)
		.getSL_names(data)
	})

.setSL_names <- selectMethod("names<-", "SimpleList")
setReplaceMethod("names", "ImageList", 
	function(x, value) {
		data <- as(x, "SimpleList", strict=FALSE)
		data <- .setSL_names(data, value=value)
		as(data, class(x))
	})

.getSL_elements <- selectMethod("[[", "SimpleList")
setMethod("[[", "ImageList",
	function(x, i, j, ...) {
		data <- as(x, "SimpleList", strict=FALSE)
		.getSL_elements(data, i=i, j=j, ...)
	})

.setSL_elements <- selectMethod("[[<-", "SimpleList")
setReplaceMethod("[[", "ImageList",
	function(x, i, j, ..., value) {
		data <- as(x, "SimpleList", strict=FALSE)
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
	function(x) {
		if ( length(x) == 0 )
			return(matrix(nrow=0, ncol=0))
		data <- as(x, "SimpleList", strict=FALSE)
		sapply(data, dim, USE.NAMES=TRUE)
	})

# show

setMethod("show", "SimpleImageList",
	function(object) {
		callNextMethod(object)
		# get class / dim information
		if ( length(object) == 0 )
			return(matrix(nrow=0, ncol=0))
		data <- as(object, "SimpleList", strict=FALSE)
		dims <- sapply(data, function(x) {
			d <- paste0(dim(x), collapse=" x ")
			paste0("<", d, ">")
		})
		dims <- paste0(dims, collapse=" ")
		classes <- paste0(sapply(data, class), collapse=" ")
		cat(sprintf("classes(%d): %s\n", length(classes), classes))
		cat(sprintf("dims(%d): %s\n", length(dims), dims))
	}
)


