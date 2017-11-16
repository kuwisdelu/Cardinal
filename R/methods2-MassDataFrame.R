
#### Methods for MassDataFrame ####
## ------------------------------------

setMethod("initialize", "MassDataFrame",
	function(.Object, mz, ...) {
		if ( missing(mz) )
			mz <- numeric()
		.Object <- .setResolutionfromMass(.Object, mz)
		callNextMethod(.Object, mz=mz, ...)
	})

MassDataFrame <- function(mz, ...,
	row.names = NULL, check.names = TRUE)
{
	if ( missing(mz) && length(list(...)) == 0 )
		return(.MassDataFrame())
	data <- DataFrame(...,
		row.names=row.names,
		check.names=check.names)
	.MassDataFrame(
		mz=mz,
		rownames=row.names,
		nrows=length(mz),
		listData=as.list(data))
}

.valid.MassDataFrame <- function(object) {
	errors <- NULL
	if ( is.unsorted(object@mz) )
		errors <- c(errors , "'mz' must be in increasing order")
	if ( any(duplicated(object@mz)) )
		errors <- c(errors , "elements of 'mz' must be unique")
	if ( length(object@mz) != object@nrows )
		errors <- c(errors , "'nrow' must equal length of 'mz'")
	if ( length(object@resolution) != 1 )
		errors <- c(errors, "length of 'resolution' must be 1")
	if ( is.null(errors) ) TRUE else errors
}

setValidity("MassDataFrame", .valid.MassDataFrame)

.setResolutionfromMass <- function(x, mz) {
	res <- .estimateMassResolution(mz)
	x@resolution <- res
	x
}

.estimateMassResolution <- function(mz) {
	if ( length(mz) == 0 || is.unsorted(mz) )
		return(NA_real_)
	bwidth <- diff(mz)
	ppm <- 1e6 * bwidth / mz[-length(mz)]
	if ( diff(range(bwidth)) < min(bwidth) ) {
		res <- c("mz" = mean(bwidth))
	} else if ( diff(range(ppm)) < min(ppm) ) {
		res <- c("ppm" = mean(ppm))
	} else {
		res <- NA_real_
	}
	res
}

setMethod("mz", "MassDataFrame",
	function(object) object@mz)

setReplaceMethod("mz", "MassDataFrame",
	function(object, value) {
		object@mz <- value
		object <- .setResolutionfromMass(object, value)
		if ( validObject(object) )
			object
	})

setMethod("resolution", "MassDataFrame",
	function(object) object@resolution)

setReplaceMethod("resolution", "MassDataFrame",
	function(object, value) {
		names(object@resolution) <- value
		if ( validObject(object) )
			object
	})

# includes 'mz' slot in the list, prefixed/affixed by ":"
setMethod("as.list", "MassDataFrame",
	function(x, use.names = TRUE, fix.mz = ":")
	{
		nmz <- paste0(fix.mz, "mz", fix.mz)
		lmz <- setNames(list(mz(x)), nmz)
		ans <- append(lmz, x@listData)
		if ( !use.names )
			names(ans) <- NULL
		ans
	})

# including 'mz' by default means they show up in 'show'
setMethod("lapply", "MassDataFrame",
	function(X, FUN, ..., with.mz = TRUE)
	{
		if ( with.mz ) {
			lapply(as.list(X), FUN=FUN, ...)
		} else {
			lapply(as.list(X@listData), FUN=FUN, ...)
		}
	})

setMethod("[", "MassDataFrame",
	function(x, i, j, ..., drop = TRUE) {
		if ( missing(i) ) {
			mz <- x@mz
		} else {
			mz <- x@mz[i]
		}
		if ( missing(j) ) {
			mcols <- mcols(x)
		} else {
			mcols <- mcols(x)[j]
		}
		x <- callNextMethod(as(x, "DataFrame"),
			i=i, j=j, ..., drop=FALSE)
		if ( missing(drop) )
			drop <- ncol(x) == 1L
		if ( drop ) {
			if (ncol(x) == 1L) 
				return(x[[1L]])
			if (nrow(x) == 1L) 
				return(as(x, "list"))
		}
		x <- .MassDataFrame(
			mz=mz,
			rownames=rownames(x),
			nrows=length(mz),
			listData=x@listData,
			elementMetadata=mcols)
		x
	})

setMethod("cbind", "MassDataFrame",
	function(..., deparse.level = 1) {
		args <- list(...)
		mz <- mz(args[[1]])
		ok <- vapply(args, function(a) 
			isTRUE(all.equal(mz(a), mz)),
			logical(1))
		if ( !all(ok) )
			stop("'mz' must match")
		x <- callNextMethod(...)
		.MassDataFrame(
			mz=mz,
			rownames=rownames(x),
			nrows=length(mz),
			listData=x@listData,
			elementMetadata=mcols(x))
	})

setMethod("rbind", "MassDataFrame",
	function(..., deparse.level = 1) {
		mz <- do.call("c",
			lapply(list(...), mz))
		x <- callNextMethod(...)
		.MassDataFrame(
			mz=mz,
			rownames=rownames(x),
			nrows=length(mz),
			listData=x@listData,
			elementMetadata=mcols(x))
	})



