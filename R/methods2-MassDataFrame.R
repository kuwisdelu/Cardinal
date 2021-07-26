
#### Methods for MassDataFrame ####
## ------------------------------------

setMethod("initialize", "MassDataFrame",
	function(.Object, ..., mz) {
		if ( missing(mz) )
			mz <- .Object@mz
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
	listData <- lapply(data, rep, length.out=length(mz))
	.MassDataFrame(
		mz=mz,
		rownames=row.names,
		nrows=length(mz),
		listData=listData)
}

.valid.MassDataFrame <- function(object) {
	errors <- NULL
	if ( object@nrows != length(object@mz) )
		errors <- c(errors , "'nrow' must equal length of 'mz'")
	if ( is.unsorted(object@mz) )
		errors <- c(errors , "'mz' must be in increasing order")
	if ( any(duplicated(object@mz)) )
		errors <- c(errors , "elements of 'mz' must be unique")
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

.estimateMassResolution <- function(mz, units, tol = 1e-6) {
	mz <- unname(mz)
	if ( missing(units) ) {
		if ( length(mz) <= 1L || is.unsorted(mz) )
			return(NA_real_)
		mzdiff <- diff(mz)
		to <- mz[-1]
		from <- mz[-length(mz)]
		ppm <- 1e6 * ((to / from ) - 1) / ((to / from) + 1)
		if ( diff(range(ppm)) <= tol * min(mz) ) {
			res <- c(ppm = roundnear(2 * median(ppm), precision=0.5))
		} else {
			res <- c(mz = round(min(mzdiff), digits=4))
		}
	} else {
		mzdiff <- median(diff(mz))
		res <- 1e6 * median(diff(mz) / mz[-1])
		res <- switch(units,
			ppm = c("ppm" = roundnear(res, precision=0.5)),
			mz = c("mz" = round(mzdiff, digits=4)))
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
		object@resolution <- value
		if ( validObject(object) )
			object
	})

setMethod("isCentroided", "MassDataFrame",
	function(object, ...) is.unsorted(diff(mz(object))))


# format names for printing includes 'mz' slot-column
setMethod("showNames", "MassDataFrame",
	function(object) c(":mz:", names(object)))

# includes 'mz' slot in the list by default
setMethod("as.list", "MassDataFrame",
	function(x, ..., slots = TRUE)
	{
		ans <- x@listData
		if ( slots )
			ans <- c(list(mz=mz(x)), ans)
		ans
	})

# subsetting

setMethod("[", "MassDataFrame",
	function(x, i, j, ..., drop = FALSE) {
		lst <- (nargs() - !missing(drop)) < 3L
		if ( lst )
			return(x[,i,drop=FALSE])
		if ( missing(i) ) {
			mz <- x@mz
		} else {
			i2 <- seq_len(nrow(x))
			i <- setNames(i2, rownames(x))[i]
			mz <- x@mz[i]
		}
		if ( missing(j) ) {
			mcols <- mcols(x)
		} else {
			j2 <- seq_len(ncol(x))
			j <- setNames(j2, colnames(x))[j]
			mcols <- mcols(x)[j]
		}
		x <- callNextMethod(as(x, "DFrame"),
			i=i, j=j, ..., drop=FALSE)
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
			elementMetadata=mcols,
			metadata=metadata(x))
		x
	})

# combining

setMethod("cbind", "MassDataFrame",
	function(..., deparse.level = 1) {
		args <- list(...)
		mz <- mz(args[[1L]])
		ok <- vapply(args, function(a) 
			isTRUE(all.equal(mz(a), mz)),
			logical(1))
		if ( !all(ok) )
			stop("'mz' must match")
		x <- do.call("cbind", lapply(args, as, "DFrame"))
		new(class(args[[1L]]),
			mz=mz,
			rownames=rownames(x),
			nrows=length(mz),
			listData=x@listData,
			elementMetadata=mcols(x))
	})

setMethod("rbind", "MassDataFrame",
	function(..., deparse.level = 1) {
		args <- list(...)
		mz <- do.call("c",
			lapply(args, mz))
		x <- do.call("rbind", lapply(args, as, "DFrame"))
		new(class(args[[1L]]),
			mz=mz,
			rownames=rownames(x),
			nrows=length(mz),
			listData=x@listData,
			elementMetadata=mcols(x))
	})



