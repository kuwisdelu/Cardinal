
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
		cat(class(object), "of length", length(object), "\n")
		if ( length(object) == 0 )
			return()
		nms <- names(object)
		if ( is.null(nms) )
			nms <- character(length(object))
		data <- as(object, "SimpleList", strict=FALSE)
		cls <- sapply(data, class)
		dms <- sapply(data, function(x) {
			d <- paste0(dim(x), collapse=" x ")
			paste0("<", d, ">")
		})
		mem <- sapply(data, function(x) format(matter::mem(x)))
		nms <- c(sprintf("names(%d):",
						length(names(object))), selectSome(nms))
		cls <- c(sprintf("class(%d):",
						length(cls)), selectSome(cls))
		dms <- c(sprintf("dim(%d):",
						length(dms)), selectSome(dms))
		mem <- c(sprintf("mem(%d):",
						length(mem)), selectSome(mem))
		out <- rbind(cls, dms, mem)
		colnames(out) <- nms
		rownames(out) <- character(nrow(out))
		print(out, quote = FALSE, right = TRUE)
	}
)


