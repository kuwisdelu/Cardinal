
#### Methods for PositionDataFrame ####
## ------------------------------------

setMethod("initialize", "PositionDataFrame",
	function(.Object, coord, run, ...) {
		if ( missing(coord) || length(coord) == 0 )
			coord <- DataFrame(x=numeric(), y=numeric())
		if ( missing(run) ) {
			if ( nrow(coord) == 0 ) {
				run <- factor()
			} else {
				run <- factor(1)
			}
		} else if ( !is.factor(run) ) {
			run <- as.factor(run)
		}
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
	if ( any(duplicated(coord)) )
		warning("rows of 'coord' are not unique")
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
		breaks <- diff(range(as.numeric(x))) / r
		breaks + 1
	}, coord, res, USE.NAMES=TRUE)
	dims
}

.estimateSpatialResolution <- function(coord) {
	res <- sapply(coord, function(x) {
		x <- sort(unique(as.numeric(x)))
		if ( length(x) < 2 )
			return(NA_real_)
		dx <- diff(x)
		rx <- min(dx, na.rm=TRUE)
		off <- (dx / rx) %% 1
		if ( sum(off) > 0 ) {
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
		object@run <- value
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
		object@resolution <- value
		if ( validObject(object) )
			object
	})

# the raster dimensions (for gridded positions only)

setMethod("dims", "PositionDataFrame",
	function(object) {
		.getDimsFromResolution(object@coord,
			object@resolution)
	})

# includes 'run' and 'coord' slot in the list by default
setMethod("as.list", "PositionDataFrame",
	function(x, use.names = TRUE, format.run=":", format.coord="coord:")
	{
		ans <- x@listData
		if ( !is.null(format.coord) ) {
			pos <- coord(x)
			if ( length(pos) > 0 )
				names(pos) <- paste0(format.coord, names(pos))
			ans <- append(as.list(pos), x@listData)
		}
		if ( !is.null(format.run) ) {
			nmr <- paste0(format.run, "run", format.run)
			run <- setNames(list(run(x)), nmr)
			ans <- append(run, ans)
		}
		if ( !use.names )
			names(ans) <- NULL
		ans
	})

# includes 'run' and 'coord' slot in the env by default
setMethod("as.env", "PositionDataFrame",
	function(x, enclos = parent.frame(2), ..., slots = TRUE)
	{
		enclos <- force(enclos)
		if ( slots ) {
			x <- as.list(x, format.run="", format.coord="")
		} else {
			x <- x@listData
		}
		as.env(x, enclos=enclos, ...)
	})

setMethod("[", "PositionDataFrame",
	function(x, i, j, ..., drop = TRUE) {
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
			elementMetadata=mcols)
		x
	})

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
		x <- callNextMethod(...)
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
		x <- callNextMethod(...)
		new(class(args[[1L]]),
			run=run,
			coord=coord,
			rownames=rownames(x),
			nrows=nrow(coord),
			listData=x@listData,
			elementMetadata=mcols(x))
	})
