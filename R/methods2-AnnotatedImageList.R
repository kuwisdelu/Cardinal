
#### Methods for AnnotatedImageList ####
## -----------------------------------

AnnotatedImageList <- function(...) {
	.Deprecated()
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
		fun <- function(x) dim(x)
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


## rbind/cbind

.bindAnnotatedImageList <- function(objects, along.frames=TRUE)
{
	if ( length(objects) == 0L )
		return(AnnotatedImageList())
	if (!along.frames) {
		images <- lapply(objects, as, Class="SimpleList")
		return(as(do.call("c", images), class(objects[[1L]])))
	}
	lens <- sapply(objects, length)
	if ( length(unique(lens)) != 1 )
		stop("the objects to bind must have the same number of elements")
	len1 <- lens[1L]
	if (len1 == 0L)
		return(AnnotatedImageList())
	namelist <- lapply(objects, names)
	vnames <- unique(unlist(namelist))
	if ( is.null(vnames) ) {
		## no names, match by position
		res <- lapply(seq_len(len1), function(index) {
			images <- lapply(objects, "[[", index)
			do.call("combine", images)
		})
	} else {
		## match by name
		ok <- all(vapply(namelist, function(x, y) identical(sort(x), y),
			logical(1), sort(vnames)))
		if ( !ok )
			stop("elements must have the same 'names'")
		res <- lapply(vnames, function(index) {
			images <- lapply(objects, "[[", index)
			do.call("combine", images)
		})
		names(res) <- vnames
	}
    as(SimpleList(res), class(objects[[1L]]))
}

setMethod("rbind", "AnnotatedImageList",
    function(..., deparse.level=1)
    {
        objects <- unname(list(...))
        .bindAnnotatedImageList(objects, along.frames=TRUE)
    }
)

setMethod("cbind", "AnnotatedImageList",
    function(..., deparse.level=1)
    {
        objects <- unname(list(...))
        .bindAnnotatedImageList(objects, along.frames=FALSE)
    }
)

setMethod("combine", "AnnotatedImageList",
	function(x, y, ...) cbind(x, y, ...))


