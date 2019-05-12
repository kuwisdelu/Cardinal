
#### Methods for AnnotatedImageList ####
## -----------------------------------

AnnotatedImageList <- function(...) {
	data <- SimpleList(...)
	fun <- function(x) {
		if ( is(x, "AnnotatedImage") ) {
			x
		} else {
			as(x, "AnnotatedImage")
		}
	}
	object <- as(endoapply(data, fun), "AnnotatedImageList")
	if ( validObject(object) )
		object
}

.valid.AnnotatedImageList <- function(object) {
	errors <- NULL
	data <- as(object, "SimpleList", strict=FALSE)
	classes_ok <- sapply(data, function(x) inherits(x, "AnnotatedImage"))
	if ( length(data) > 0 && !all(classes_ok) )
		errors <- c(errors , "elements must be of class 'AnnotatedImage'")
	num_frames <- sapply(data, function(x) numberOfFrames(x))
	if ( length(data) > 0 && length(unique(num_frames)) > 1 )
		errors <- c(errors , "elements must have the same number of frames")
	if ( is.null(errors) ) TRUE else errors
}

setValidity("AnnotatedImageList", .valid.AnnotatedImageList)

setMethod("dim", "AnnotatedImageList",
	function(x) {
		if ( length(x) == 0 ) {
			0L
		} else {
			c(numberOfFrames(x[[1L]]), length(x))
		}
	})

setMethod("dims", "AnnotatedImageList",
	function(x) {
		if ( length(x) == 0 )
			return(matrix(nrow=0, ncol=0))
		data <- as(x, "SimpleList", strict=FALSE)
		fun <- function(x) dim(x)[c(1,2)]
		sapply(data, fun, USE.NAMES=TRUE)
	})


## 2D-Subsetting for AnnotatedImageList

# i subsets the frames; j subsets the list

.subsetAnnotatedImageList <- function(x, i, j, drop = FALSE)
{
	data <- as(x, "SimpleList", strict=FALSE)
	if ( !missing(j) )
		data <- data[j]
	if ( !missing(i) ) {
		fun <- function(x) {
			switch(length(dim(x)),
				stop("'[' on SimpleImageArrayList with 1 dimension not supported"),
				x[, , drop=drop],
				x[, , i, drop=drop],
				x[, , , i, drop=drop],
				stop("'[' on SimpleImageArrayList with >4 dimensions not supported"))
		}
		data <- endoapply(data, fun)
	}
	as(data, class(x))
}

setMethod("[", "AnnotatedImageList",
	function(x, i, j, ..., drop = FALSE) {
		lst <- (nargs() - !missing(drop)) < 3L
		if ( lst ) {
			.subsetAnnotatedImageList(x, j=i, drop=drop)
		} else {
			.subsetAnnotatedImageList(x, i=i, j=j, drop=drop)
		}
	})


