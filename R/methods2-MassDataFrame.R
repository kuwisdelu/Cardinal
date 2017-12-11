
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

# includes 'mz' slot in the list by default
setMethod("as.list", "MassDataFrame",
	function(x, use.names = TRUE, format.mz = ":")
	{
		ans <- x@listData
		if ( !is.null(format.mz) ) {
			nmz <- paste0(format.mz, "mz", format.mz)
			lmz <- setNames(list(mz(x)), nmz)
			ans <- append(lmz, ans)
		}
		if ( !use.names )
			names(ans) <- NULL
		ans
	})

# includes 'mz' slot in the env by default
setMethod("as.env", "MassDataFrame",
	function(x, enclos = parent.frame(2), ..., slots = TRUE)
	{
		enclos <- force(enclos)
		if ( slots ) {
			x <- as.list(x, format.mz="")
		} else {
			x <- x@listData
		}
		as.env(x, enclos=enclos, ...)
	})

setMethod("[", "MassDataFrame",
	function(x, i, j, ..., drop = TRUE) {
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
		x <- callNextMethod(as(x, "DataFrame"),
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
			elementMetadata=mcols)
		x
	})

setMethod("cbind", "MassDataFrame",
	function(..., deparse.level = 1) {
		args <- list(...)
		mz <- mz(args[[1L]])
		ok <- vapply(args, function(a) 
			isTRUE(all.equal(mz(a), mz)),
			logical(1))
		if ( !all(ok) )
			stop("'mz' must match")
		x <- callNextMethod(...)
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
		x <- callNextMethod(...)
		new(class(args[[1L]]),
			mz=mz,
			rownames=rownames(x),
			nrows=length(mz),
			listData=x@listData,
			elementMetadata=mcols(x))
	})



