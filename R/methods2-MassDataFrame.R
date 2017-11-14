
#### Methods for MassDataFrame ####
## ------------------------------------

setMethod("initialize", "MassDataFrame",
	function(.Object, mz, ...) {
		.Object <- .setResolutionfromMass(.Object, mz)
		callNextMethod(.Object, mz=mz, ...)
	})

MassDataFrame <- function(mz, ...,
	row.names = NULL, check.names = TRUE)
{
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

setMethod("as.list", "MassDataFrame",
	function(x, use.names = TRUE)
	{
		ans <- append(list(":mz:"=mz(x)), x@listData)
		if ( !use.names )
			names(ans) <- NULL
		ans
	})

setMethod("[", "MassDataFrame",
	function(x, i, j, ..., drop = TRUE) {
		if ( missing(i) ) {
			mz <- x@mz
		} else {
			mz <- x@mz[i]
		}
		x <- callNextMethod(DataFrame(x@listData),
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
			nrows=nrow(x),
			listData=as.list(x))
		x
	})

# still need to implement
setMethod("rbind", "MassDataFrame", callNextMethod)

# still need to implement
setMethod("cbind", "MassDataFrame", callNextMethod)


