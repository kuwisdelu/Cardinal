
#### Methods for ImageArrayList ####
## -----------------------------

ImageArrayList <- function(data) {
	if ( !is(data, "SimpleList") && !is(data, "ImageList") ) {
		if ( is.list(data) || !is.null(dim(data)) ) {
			data <- SimpleList(data)
		} else {
			stop("'data' must be a SimpleList, list or array-like")
		}
	}
	object <- as(data, "SimpleImageArrayList", strict=FALSE)
	if ( is.null(names(object)) )
		names(object) <- paste0("data", (seq_along(object) - 1L))
	if ( validObject(object) )
		object
}

.valid.ImageArrayList <- function(object) {
	errors <- NULL
	data <- as(object, "SimpleList", strict=FALSE)
	dimlengths <- sapply(data, function(x) length(dim(x)))
	if ( length(data) > 0 && length(unique(dimlengths)) > 1 )
		errors <- c(errors , "elements must have the same number of dimensions")
	dims <- sapply(data, function(x) dim(x)[c(1,2)])
	if ( length(data) > 0 && !all(dims == dims[,1]) )
		errors <- c(errors , "elements must have the same 'nrow' and 'ncol'")
	if ( is.null(errors) ) TRUE else errors
}

setValidity("ImageArrayList", .valid.ImageArrayList)


## 2D-Subsetting for SimpleImageArrayList

# Note that drop = NULL can be used here to do endomorphic
# subsetting of 'matter' objects -- this does not seem
# to cause problems for in-memory R matrices or DelayedArray
# arrays. Fails for Matrix and big.matrix. Solution is
# to wrap objects in DelayedArray or virtual_mat, or
# simply set drop = FALSE manually?

.subsetSimpleImageArrayList <- function(x, i, j, drop = FALSE)
{
	if ( isTRUE(drop) )
		drop <- FALSE
	if (!missing(i) && !missing(j)) {
		fun <- function(x) {
			if ( !is.matter(x) )
				drop <- FALSE
			switch(length(dim(x)),
				stop("'[' on SimpleImageArrayList with 1 dimension not supported"),
				x[i, j, drop=drop],
				x[i, j, , drop=drop],
				x[i, j, , , drop=drop],
				stop("'[' on SimpleImageArrayList with >4 dimensions not supported"))
		}
	} else if (!missing(i)) {
		fun <- function(x) {
			if ( !is.matter(x) )
				drop <- FALSE
			switch(length(dim(x)),
				stop("'[' on SimpleImageArrayList with 1 dimension not supported"),
				x[i, , drop=drop],
				x[i, , , drop=drop],
				x[i, , , , drop=drop],
				stop("'[' on SimpleImageArrayList with >4 dimensions not supported"))
		}
	} else if (!missing(j)) {
		fun <- function(x) {
			if ( !is.matter(x) )
				drop <- FALSE
			switch(length(dim(x)),
				stop("'[' on SimpleImageArrayList with 1 dimension not supported"),
				x[, j, drop=drop],
				x[, j, , drop=drop],
				x[, j, , , drop=drop],
				stop("'[' on SimpleImageArrayList with >4 dimensions not supported"))
		}
	}
	data <- as(x, "SimpleList", strict=FALSE)
	as(endoapply(data, fun), class(x))
}

setMethod("[", "ImageArrayList",
	function(x, i, j, ..., drop = FALSE)
		.subsetSimpleImageArrayList(x, i, j, drop))

.replaceSimpleImageArrayList <- function(x, i, j, value)
{
	if (!missing(i) && !missing(j)) {
		fun <- function(x, value) {
			switch(length(dim(x)),
				stop("'[<-' on SimpleImageArrayList with 1 dimension not supported"),
				x[i, j] <- value,
				x[i, j, ] <- value,
				x[i, j, , ] <- value,
				stop("'[<-' on SimpleImageArrayList with >4 dimensions not supported"))
			x
		}
	} else if (!missing(i)) {
		fun <- function(x, value) {
			switch(length(dim(x)),
				stop("'[<-' on SimpleImageArrayList with 1 dimension not supported"),
				x[i, ] <- value,
				x[i, , ] <- value,
				x[i, , , ] <- value,
				stop("'[<-' on SimpleImageArrayList with >4 dimensions not supported"))
			x
		}
	} else if (!missing(j)) {
		fun <- function(x, value) {
			switch(length(dim(x)),
				stop("'[<-' on SimpleImageArrayList with 1 dimension not supported"),
				x[, j] <- value,
				x[, j, ] <- value,
				x[, j, , ] <- value,
				stop("'[<-' on SimpleImageArrayList with >4 dimensions not supported"))
			x
		}
	}
	a <- as(x, "SimpleList", strict=FALSE)
	v <- as(value, "SimpleList", strict=FALSE)
	as(mendoapply(fun, x=a, value=v), class(x))
}

setReplaceMethod("[", "ImageArrayList",
	function(x, i, j, ..., value) 
		.replaceSimpleImageArrayList(x, i, j, value))

## rbind/cbind

# 'images' is assumed to be an unnamed list of length >= 1
.bindImageArrays <- function(images, along.cols=FALSE) {
	if ( along.cols ) {
		bindfun <- "cbind"
	} else {
		bindfun <- "rbind"
	}
	do.call(bindfun, images)
}

.bindSimpleImageArrayList <- function(objects, along.cols=FALSE)
{
	if ( length(objects) == 0L )
		return(ImageArrayList())
	lens <- sapply(objects, length)
	if ( length(unique(lens)) != 1 )
		stop("the objects to bind must have the same number of elements")
	len1 <- lens[1L]
	if (len1 == 0L)
		return(ImageArrayList())
	namelist <- lapply(objects, names)
	vnames <- unique(unlist(namelist))
	if ( is.null(vnames) ) {
		## no names, match by position
		res <- lapply(seq_len(len1), function(index) {
			images <- lapply(objects, "[[", index)
			.bindImageArrays(images, along.cols=along.cols)
		})
	} else {
		## match by name
		ok <- all(vapply(namelist, function(x, y) identical(sort(x), y),
			logical(1), sort(vnames)))
		if ( !ok )
			stop("elements must have the same 'names'")
		res <- lapply(vnames, function(index) {
			images <- lapply(objects, "[[", index)
			.bindImageArrays(images, along.cols=along.cols)
		})
		names(res) <- vnames
	}
    as(SimpleList(res), class(objects[[1L]]))
}

setMethod("rbind", "ImageArrayList",
    function(..., deparse.level=1)
    {
        objects <- unname(list(...))
        .bindSimpleImageArrayList(objects, along.cols=FALSE)
    }
)

setMethod("cbind", "ImageArrayList",
    function(..., deparse.level=1)
    {
        objects <- unname(list(...))
        .bindSimpleImageArrayList(objects, along.cols=TRUE)
    }
)
