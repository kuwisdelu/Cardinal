
#### Methods for PositionDataFrame ####
## ------------------------------------

setMethod("initialize", "PositionDataFrame",
	function(.Object, coord, ...) {
		if ( missing(coord) )
			coord <- DataFrame(x=numeric(), y=numeric())
		.Object <- .setResolutionfromCoord(.Object, coord)
		callNextMethod(.Object, coord=coord, ...)
	})

PositionDataFrame <- function(coord, ...,
	row.names = NULL, check.names = TRUE)
{
	data <- DataFrame(...,
		row.names=row.names,
		check.names=check.names)
	.PositionDataFrame(
		coord=DataFrame(coord),
		rownames=row.names,
		nrows=nrow(data),
		listData=as.list(data))
}

.valid.PositionDataFrame <- function(object) {
	errors <- NULL
	if ( length(object@coord) < 2 )
		errors <- c(errors , "'coord' must have at least 2 columns")
	if ( any(duplicated(object@coord)) )
		errors <- c(errors, "rows of 'coord' must be unique")
	if ( !all(sapply(object@coord, mode) == "numeric") )
		errors <- c(errors, "columns of 'coord' must have mode 'numeric'")
	if ( any(sapply(object@coord, anyNA)) )
		errors <- c(errors, "NA not allowed in 'coord'")
	if ( length(object@gridded) != 1 )
		errors <- c(errors, "'gridded' must have length 1")
	if ( length(object@resolution) != length(object@coord) )
		errors <- c(errors, "length of 'resolution' and 'coord' must match")
	if ( is.null(errors) ) TRUE else errors
}

setValidity("PositionDataFrame", .valid.PositionDataFrame)

.setResolutionfromCoord <- function(x, coord) {
	res <- .estimateSpatialResolution(coord)
	if ( any(is.na(res)) ) {
		x@gridded <- FALSE
	} else {
		x@gridded <- TRUE
	}
	x@resolution <- res
	x
}

.getDimsFromResolution <- function(coord, res) {
	dims <- mapply(function(x, r) {
		breaks <- diff(range(x)) / r
		breaks + 1
	}, coord, res, USE.NAMES=TRUE)
	dims
}

.estimateSpatialResolution <- function(coord) {
	res <- sapply(coord, function(x) {
		x <- sort(unique(x))
		if ( length(x) < 2 )
			return(NA_real_)
		dx <- diff(x)
		rx <- min(dx)
		off <- (dx / rx) %% 1
		if ( sum(off) > 0 ) {
			NA_real_
		} else {
			rx
		}
	})
	res
}

setMethod("coord", "PositionDataFrame",
	function(object) object@coord)

setReplaceMethod("coord", "PositionDataFrame",
	function(object, value) {
		object@coord <- value
		object <- .setResolutionfromCoord(object, value)
		if ( validObject(object) )
			object
	})

setMethod("coordnames", "PositionDataFrame",
	function(x) names(x@coord))

setReplaceMethod("coordnames", "PositionDataFrame",
	function(x, value) {
		names(x@coord) <- value
		if ( validObject(x) )
			x
	})

setMethod("gridded", "PositionDataFrame",
	function(obj) obj@gridded)

setReplaceMethod("gridded", "PositionDataFrame",
	function(obj, value) {
		names(obj@gridded) <- value
		if ( validObject(obj) )
			obj
	})

setMethod("resolution", "PositionDataFrame",
	function(object) object@resolution)

setReplaceMethod("resolution", "PositionDataFrame",
	function(object, value) {
		names(object@resolution) <- value
		if ( validObject(object) )
			object
	})

setMethod("dims", "PositionDataFrame",
	function(object) {
		.getDimsFromResolution(object@coord,
			object@resolution)
	})

# includes 'coord' slot in the list, prefixed by "coord:"
setMethod("as.list", "PositionDataFrame",
	function(x, use.names = TRUE, fix.coord="coord:")
	{
		pos <- coord(x)
		names(pos) <- paste0(fix.coord, names(pos))
		ans <- append(as.list(pos), x@listData)
		if ( !use.names )
			names(ans) <- NULL
		ans
	})

# including 'coord' by default means they show up in 'show'
setMethod("lapply", "PositionDataFrame",
	function(X, FUN, ..., with.coord = TRUE)
	{
		if ( with.coord ) {
			lapply(as.list(X), FUN=FUN, ...)
		} else {
			lapply(as.list(X@listData), FUN=FUN, ...)
		}
	})

setMethod("[", "PositionDataFrame",
	function(x, i, j, ..., drop = TRUE) {
		if ( missing(i) ) {
			coord <- x@coord
		} else {
			coord <- x@coord[i,]
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
		x <- .PositionDataFrame(
			coord=coord,
			rownames=rownames(x),
			nrows=nrow(x),
			listData=x@listData,
			elementMetadata=mcols)
		x
	})

setMethod("cbind", "PositionDataFrame",
	function(..., deparse.level = 1) {
		args <- list(...)
		coord <- coord(args[[1]])
		ok <- vapply(args, function(a) 
			isTRUE(all.equal(coord(a), coord)),
			logical(1))
		if ( !all(ok) )
			stop("'coord' must match")
		x <- callNextMethod(...)
		.PositionDataFrame(
			coord=coord,
			rownames=rownames(x),
			nrows=nrow(x),
			listData=x@listData,
			elementMetadata=mcols(x))
	})

setMethod("rbind", "PositionDataFrame",
	function(..., deparse.level = 1) {
		coord <- do.call("rbind",
			lapply(list(...), coord))
		x <- callNextMethod(...)
		.PositionDataFrame(
			coord=coord,
			rownames=rownames(x),
			nrows=nrow(x),
			listData=x@listData,
			elementMetadata=mcols(x))
	})
