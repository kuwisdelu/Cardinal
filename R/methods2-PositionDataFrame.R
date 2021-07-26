
#### Methods for PositionDataFrame ####
## ------------------------------------

setMethod("initialize", "PositionDataFrame",
	function(.Object, ..., coord, run) {
		if ( missing(coord) )
			coord <- .Object@coord
		if ( missing(run) )
			run <- .Object@run
		if ( length(run) != nrow(coord) )
			run <- rep(run, length.out=nrow(coord))
		.Object <- .setResolutionfromCoord(.Object, coord)
		callNextMethod(.Object, run=run, coord=coord, ...)
	})

PositionDataFrame <- function(coord, run, ...,
	row.names = NULL, check.names = TRUE)
{
	if ( missing(coord) && length(list(...)) == 0 )
		return(.PositionDataFrame())
	coord <- DataFrame(coord)
	if ( missing(run) ) {
		run <- rep(factor("run0"), length.out=nrow(coord))
		if ( any(duplicated(coord)) )
			warning("rows of 'coord' are not unique")
	} else {
		run <- as.factor(run)
		if ( any(duplicated(cbind(data.frame(run=run), coord))) )
			warning("rows of 'coord' + 'run' are not unique")
	}
	data <- DataFrame(...,
		row.names=row.names,
		check.names=check.names)
	listData <- lapply(data, rep, length.out=nrow(coord))
	.PositionDataFrame(
		coord=coord,
		run=run,
		rownames=row.names,
		nrows=nrow(coord),
		listData=listData)
}

.valid.PositionDataFrame <- function(object) {
	errors <- NULL
	if ( object@nrows != length(object@run) )
		errors <- c(errors , "'nrow' must match length of 'run'")
	if ( object@nrows != nrow(object@coord) )
		errors <- c(errors , "'nrow' must match number of rows of 'coord'")
	if ( length(object@coord) < 2 )
		errors <- c(errors , "'coord' must have at least 2 columns")
	if ( !all(sapply(object@coord, mode) == "numeric") )
		errors <- c(errors, "columns of 'coord' must have mode 'numeric'")
	if ( length(object@gridded) != 1 )
		errors <- c(errors, "'gridded' must have length 1")
	if ( length(object@resolution) != length(object@coord) )
		errors <- c(errors, "length of 'resolution' and 'coord' must match")
	if ( any(names(object@resolution) != names(object@coord)) )
		errors <- c(errors, "names of 'resolution' and 'coord' must match")
	if ( is.null(errors) ) TRUE else errors
}

setValidity("PositionDataFrame", .valid.PositionDataFrame)

.setResolutionfromCoord <- function(x, coord) {
	res <- .estimateSpatialResolution(coord)
	dims <- .getDimsFromResolution(coord, res)
	if ( any(is.na(dims)) ) {
		x@gridded <- FALSE
	} else {
		x@gridded <- TRUE
	}
	x@resolution <- res
	x
}

.getDimsFromResolution <- function(coord, res) {
	dims <- mapply(function(x, r) {
		if ( length(x) < 1L )
			return(NA_real_)
		xrange <- range(as.numeric(x))
		if ( xrange[1L] == xrange[2L] )
			return(1)
		breaks <- diff(xrange) / r
		breaks + 1
	}, coord, res, USE.NAMES=TRUE)
	dims
}

.estimateSpatialResolution <- function(coord, tol=1e-6) {
	res <- sapply(coord, function(x) {
		x <- sort(unique(as.numeric(x)))
		if ( length(x) < 2 )
			return(NA_real_)
		dx <- diff(x)
		rx <- min(dx, na.rm=TRUE)
		off <- (dx / rx) %% 1
		if ( any(off > tol) ) {
			NA_real_
		} else {
			rx
		}
	})
	res
}

setMethod("run", "PositionDataFrame",
	function(object) object@run)

setReplaceMethod("run", "PositionDataFrame",
	function(object, value) {
		if ( length(value) != nrow(object) )
			value <- rep(value, length.out=nrow(object))
		object@run <- as.factor(value)
		if ( validObject(object) )
			object
	})

setMethod("runNames", "PositionDataFrame",
	function(object) levels(object@run))

setReplaceMethod("runNames", "PositionDataFrame",
	function(object, value) {
		levels(object@run) <- value
		if ( validObject(object) )
			object
	})

setMethod("coord", "PositionDataFrame",
	function(object) object@coord)

setReplaceMethod("coord", "PositionDataFrame",
	function(object, value) {
		object@coord <- value
		object <- .setResolutionfromCoord(object, value)
		if ( validObject(object) )
			object
	})

setMethod("coordLabels", "PositionDataFrame",
	function(object) names(object@coord))

setReplaceMethod("coordLabels", "PositionDataFrame",
	function(object, value) {
		names(object@coord) <- value
		if ( validObject(object) )
			object
	})

setMethod("coordinates", "PositionDataFrame",
	function(obj, ...) obj@coord)

setReplaceMethod("coordinates", "PositionDataFrame",
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

# whether the positions are gridded

setMethod("gridded", "PositionDataFrame",
	function(obj) obj@gridded)

setReplaceMethod("gridded", "PositionDataFrame",
	function(obj, value) {
		obj@gridded <- value
		if ( validObject(obj) )
			obj
	})

# the spatial resolution

setMethod("resolution", "PositionDataFrame",
	function(object) object@resolution)

setReplaceMethod("resolution", "PositionDataFrame",
	function(object, value) {
		oldres <- object@resolution
		object@resolution <- value
		fun <- function(oldres, newres, x) {
			if ( is.na(oldres) || is.na(newres) ) {
				x
			} else {
				newres * x / oldres
			}
		}
		names(object@resolution) <- names(oldres)
		object@coord[] <- mapply(fun, oldres,
			object@resolution, object@coord, SIMPLIFY=FALSE)
		if ( validObject(object) )
			object
	})

# the raster dimensions (for gridded positions only)

setMethod("dims", "PositionDataFrame",
	function(x) .getDimsFromResolution(x@coord, x@resolution))

# format names for printing includes 'coord' slot-columns
setMethod("showNames", "PositionDataFrame",
	function(object) {
		cnm <- paste0("coord:", names(coord(object)))
		c(":run:", cnm, names(object))
	})

setMethod("is3D", "PositionDataFrame",
	function(object) ncol(coord(object)) >= 3L)

# includes 'run' and 'coord' slot in the list by default
setMethod("as.list", "PositionDataFrame",
	function(x, use.names = TRUE, slots = TRUE)
	{
		ans <- x@listData
		if ( slots )
			ans <- c(list(run=run(x)), as.list(coord(x)), ans)
		ans
	})

# subsetting

setMethod("[", "PositionDataFrame",
	function(x, i, j, ..., drop = FALSE) {
		lst <- (nargs() - !missing(drop)) < 3L
		if ( lst )
			return(x[,i,drop=FALSE])
		if ( missing(i) ) {
			run <- x@run
			coord <- x@coord
		} else {
			i2 <- seq_len(nrow(x))
			i <- setNames(i2, rownames(x))[i]
			run <- x@run[i]
			coord <- x@coord[i,]
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
		x <- .PositionDataFrame(
			run=droplevels(run),
			coord=droplevels(coord),
			rownames=rownames(x),
			nrows=nrow(coord),
			listData=x@listData,
			elementMetadata=mcols,
			metadata=metadata(x))
		x
	})

# combining

setMethod("cbind", "PositionDataFrame",
	function(..., deparse.level = 1) {
		args <- list(...)
		# check that runs match
		run <- run(args[[1L]])
		ok <- vapply(args, function(a) 
			isTRUE(all.equal(run(a), run)),
			logical(1))
		if ( !all(ok) )
			stop("'run' must match")
		# check that coords match
		coord <- coord(args[[1L]])
		ok <- vapply(args, function(a) 
			isTRUE(all.equal(coord(a), coord)),
			logical(1))
		if ( !all(ok) )
			stop("'coord' must match")
		x <- do.call("cbind", lapply(args, as, "DFrame"))
		new(class(args[[1L]]),
			run=run,
			coord=coord,
			rownames=rownames(x),
			nrows=nrow(coord),
			listData=x@listData,
			elementMetadata=mcols(x))
	})

setMethod("rbind", "PositionDataFrame",
	function(..., deparse.level = 1) {
		args <- list(...)
		run <- lapply(args, "run")
		levs <- unique(unlist(lapply(run, levels), use.names=FALSE))
		run <- do.call("c", lapply(run, as.character))
		run <- factor(run, levels=levs)
		coord <- lapply(args, "coord")
		coord <- do.call("rbind", coord)
		x <- do.call("rbind", lapply(args, as, "DFrame"))
		new(class(args[[1L]]),
			run=run,
			coord=coord,
			rownames=rownames(x),
			nrows=nrow(coord),
			listData=x@listData,
			elementMetadata=mcols(x))
	})
