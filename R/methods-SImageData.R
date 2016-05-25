
setMethod("initialize", "SImageData",
	function(.Object,
		coord = data.frame(x=numeric(), y=numeric()),
		positionArray = array(0, dim=c(x=0, y=0)),
		dim = c(0, 0),
		dimnames = list(NULL, NULL),
		...)
	{
		.Object <- callNextMethod(.Object, ...)
		if ( is.null(.Object[["iData"]]) ) {
			.Object@data[["iData"]] <- matrix(nrow=dim[1], ncol=dim[2])
		} else {
			data <- .Object@data[["iData"]]
			if ( length(dim(data)) > 2 ) {
				dims <- dim(data)[-1]
				if ( is.null(names(dims)) || any(nchar(dims) == 0) ) {
					if ( length(dims) %in% c(2,3) ) {
						names(dims) <- c("x", "y", "z")[seq_along(dims)]
					} else {
						names(dims) <- paste("dim", seq_along(dims), sep="")
					}
				}
				positionArray <- array(seq_len(prod(dims)), dim=dims,
					dimnames=lapply(dims, seq_len))
				dim(data) <- c(dim(data)[1], prod(dims))
				coord <- expand.grid(lapply(dims, seq_len))
				names(coord) <- names(dims)
				.Object@data[["iData"]] <- data
			}
		}
		if ( is.null(dimnames) )
			dimnames <- list(NULL, NULL)
		if ( is.null(dim(data)) ) {
			dim <- c(0,0)
		} else {
			dim <- dim(data)
		}
		.Object@coord <- coord
		.Object@positionArray <- positionArray
		.Object@dim <- dim
		.Object@dimnames <- dimnames
		.Object
	})

SImageData <- function(
	data = Hashmat(nrow=0, ncol=0),
	coord = expand.grid(
		x = seq_len(ncol(data)),
		y = seq_len(ifelse(ncol(data) > 0, 1, 0))),
	storageMode = "immutableEnvironment",
	positionArray = generatePositionArray(coord),
	dimnames = NULL,
	...)
{
	.SImageData(iData=data,
		coord=coord,
		storageMode=storageMode,
		positionArray=positionArray,
		dim=dim,
		dimnames=dimnames,
		...)
}

setValidity("SImageData", function(object) {
	msg <- validMsg(NULL, NULL)
	if ( object@storageMode != "immutableEnvironment" )
		msg <- validMsg(msg, "storageMode must be 'immutableEnvironment' for an SImageData")
	names <- ls(object@data)
	if ( !all(sapply(names, function(nm) length(dim(object@data[[nm]])) == 2)) )
		msg <- validMsg(msg, "all data elements must be a matrix-like object ('dims' of length 2)")
	ncols <- sapply(names, function(nm) ncol(object@data[[nm]]))
	if ( !all(sapply(ncols, function(nc) nc == ncols[1] && nc == object@dim[2])) )
		msg <- validMsg(msg, "all elements must have an equal number of columns")
	nrows <- sapply(names, function(nm) nrow(object@data[[nm]]))
	if ( !all(sapply(nrows, function(nr) nr == nrows[1] && nr == object@dim[1])) )
		msg <- validMsg(msg, "all elements must have an equal number of rows")
	if ( sum(!is.na(object@positionArray)) > 0 && any(!is.integer(object@positionArray[!is.na(object@positionArray)])) )
		msg <- validMsg(msg, "positionArray must contain only integers and NAs")
	if ( any(sapply(names, function(nm) ncol(object@data[[nm]])) != sum(!is.na(object@positionArray))) )
		msg <- validMsg(msg, "number of non-NA indices in positionArray must match number of cols of data elements")
	if ( any(nrow(object@coord) != sum(!is.na(object@positionArray))) )
		msg <- validMsg(msg, "number of non-NA indices in positionArray must match number of rows of coord")
	dmn <- object@dimnames
	if ( length(dmn) != 2 )
		msg <- validMsg(msg, paste("length of 'dimnames' [",
			length(dmn), "] must match that of 'dims' [2]", sep=""))
	if ( !is.null(dmn[[1]]) && length(dmn[[1]]) != nrows[1] )
		msg <- validMsg(msg, paste("length of 'dimnames' [",
			length(dmn[[1]]), "] not equal to array extent", sep=""))
	if ( !is.null(dmn[[2]]) && length(dmn[[2]]) != ncols[1] )
		msg <- validMsg(msg, paste("length of 'dimnames' [",
			length(dmn[[2]]), "] not equal to array extent", sep=""))
	if ( !isTRUE(all.equal(object@positionArray, generatePositionArray(object@coord))) )
		warning("positions are out of sync; run 'object <- regeneratePositions(object)' to resync")
	if ( is.null(msg) ) TRUE else msg
})

setMethod("iData", "SImageData",
	function(object) object[["iData"]])

setReplaceMethod("iData", "SImageData",
	function(object, value) {
		object[["iData"]] <- value
		if ( validObject(object) )
			object			
	})

setMethod("regeneratePositions", "SImageData",
	function(object) {
		object@positionArray <- generatePositionArray(object@coord)
		object
	})

setMethod("coord", "SImageData",
	function(object) object@coord)

setReplaceMethod("coord", "SImageData",
	function(object, value) {
		object@coord <- value
		regeneratePositions(object)
	})

setMethod("positionArray", "SImageData",
	function(object) object@positionArray)

setReplaceMethod("positionArray", "SImageData",
	function(object, value) {
		object@positionArray <- value
		if ( validObject(object) )
			object			
	})

setMethod("featureNames", "SImageData",
	function(object) object@dimnames[[1]])

setReplaceMethod("featureNames", "SImageData",
	function(object, value) {
		object@dimnames[[1]] <- as.character(value)
		object
	})

setMethod("pixelNames", "SImageData",
	function(object) object@dimnames[[2]])

setReplaceMethod("pixelNames", "SImageData",
	function(object, value) {
		object@dimnames[[2]] <- as.character(value)
		row.names(object@coord) <- value
		object
	})

setMethod("dim", "SImageData",
	function(x) c(Features=x@dim[1], dim(x@positionArray)))

setMethod("dims", "SImageData",
	function(object) {
		names <- ls(object@data, all.names=TRUE)
		if ( length(names) > 0 ) {
			nr <- sapply(names, function(nm) nrow(object@data[[nm]]))
			dm <- rep(dim(object@positionArray), length(names))
			dim(dm) <- c(length(dim(object@positionArray)), length(names))
			dims <- rbind(nr, dm)
			if ( is.null(names(dim(object@positionArray))) ) {
				rownames(dims) <- c("Features", rep("", length(dim(object@positionArray))))
			} else {
				rownames(dims) <- c("Features", names(dim(object@positionArray)))
			}
			colnames(dims) <- names
			dims
		} else {
			matrix(nrow=0, ncol=0)
		}
	})

setMethod("[", "SImageData", function(x, i, j, ..., drop) {
	if ( !missing(drop) && is.na(drop) ) {
		# return a subset of class SImageData
		if ( missing(i) ) i <- seq_len(dim(x)[1])
		if ( missing(j) ) j <- seq_len(dim(x)[2])
		names <- ls(x@data, all.names=TRUE)
		for ( nm in names ) {
			if ( isS4(x[[nm]]) ) {
				x[[nm]] <- x[[nm]][i,j,drop=NA]
			} else {
				x[[nm]] <- x[[nm]][i,j,drop=FALSE]
			}
		}
		x@coord <- x@coord[j,]
		x@positionArray <- generatePositionArray(x@coord)
		x@dim <- c(length(i), length(j))
		x@dimnames <- list(x@dimnames[[1]][i], x@dimnames[[2]][j])
		x
	} else {
		# reconstruct the data cube and return subset as an array
		nargs <- nargs() - 1 - !missing(drop)
		if ( nargs != length(dim(x)) && !(nargs == 1 && missing(i)) )
			.stop("incorrect number of dimensions")
		if ( missing(drop) ) drop <- TRUE
		args <- lapply(dim(x), function(dm) seq_len(dm))
		if ( !missing(i) ) args[[1]] <- i
		if ( !missing(j) ) args[[2]] <- j
		if ( nargs > 2 ) {
			dots <- match.call(expand.dots=FALSE)$...
			nonmissing <- sapply(dots, nchar) > 0 # changed from !is.symbol
			if ( sum(nonmissing) > 0 )
				args[c(FALSE,FALSE,nonmissing)] <- lapply(dots[nonmissing], eval)
		}
		inds <- do.call("[", c(list(x@positionArray), args[-1], drop=FALSE))
		cube <- matrix(NA, nrow=length(args[[1]]), ncol=length(inds))
		cube[,!is.na(inds)] <- iData(x)[args[[1]],inds[!is.na(inds)],drop=FALSE]
		dim(cube) <- c(dim(cube)[1], dim(inds))
		names(dim(cube)) <- c("Features", names(dim(x@positionArray)))
		if ( drop && all(dim(cube) == 1) )
			cube <- as.vector(cube)
		if ( drop && any(dim(cube) == 1) )
			dim(cube) <- dim(cube)[dim(cube) != 1]
		if ( drop && length(dim(cube)) == 1 )
			cube <- as.vector(cube)
		cube
	}
})

generatePositionArray <- function(coord, dim, dimnames) {
	if ( nrow(coord) == 0 ) {
		dim <- rep(0, ncol(coord))
		names(dim) <- names(coord)
		return(array(0, dim=dim))
	}
	coord <- data.frame(lapply(coord, as.integer))
	if ( missing(dim) )
		dim <- sapply(coord, max)
	if ( missing(dimnames) )
		dimnames <- lapply(dim, seq_len)
	positionArray <- array(1:prod(dim), dim=dim, dimnames=dimnames)
	f <- function(...) positionArray[...]
	fill <- apply(coord, 1, function(xyz) do.call(f, as.list(xyz)))
	positionArray <- array(NA, dim=dim, dimnames=dimnames)
	positionArray[fill] <- seq_len(nrow(coord))
	positionArray
}

setMethod("combine",
	signature = c(x = "SImageData", y = "SImageData"),
	function(x, y, ...) {
		if ( length(ls(x@data)) != length(ls(y@data)) )
			.warning("SImageData have different numbers of elements:\n\t",
				paste(ls(x@data), collapse=" "), "\n\t",
				paste(ls(y@data), collapse=" "))
		if ( !all(ls(x@data) == ls(y@data)) )
			.warning(paste("SImageData have different element names:",
				paste(ls(x@data), collapse=" "),
				paste(ls(y@data), collapse=" "), sep="\n\t"))
		if ( prod(dim(y)) == 0 )
			return(x)
		if ( prod(dim(x)) == 0 )
			return(y)
		coord <- rbind(x@coord, y@coord)
		if ( any(duplicated(coord)) )
			.stop("SImageData contain pixels with duplicate coordinates")
		positionArray <- generatePositionArray(coord)
		data <- new.env(parent=emptyenv())
		for ( nm in intersect(ls(x@data), ls(y@data)) ) {
			tryCatch(data[[nm]] <- cbind(x[[nm]], y[[nm]]),
				error=function(e) {
					.warning(paste0("Error combining '", nm, "'.",
						" It will be dropped from the result."))
				})
		}
		dimnames <- list(dimnames(x)[[1]],
			c(dimnames(x)[[2]], dimnames(y)[[2]]))
		new(class(x),
			data=data,
			coord=coord,
			positionArray=positionArray,
			storageMode=x@storageMode,
			dim=dim(positionArray),
			dimnames=dimnames)
	})

# setMethod("combine",
# 	signature = c(x = "SImageData", y = "SImageData"),
# 	function(x, y, ...) {
# 		if ( length(ls(x@data)) != length(ls(y@data)) )
# 			.stop("SImageData have different numbers of elements:\n\t",
# 				paste(ls(x@data), collapse=" "), "\n\t",
# 				paste(ls(y@data), collapse=" "))
# 		if ( !all(ls(x@data) == ls(y@data)) )
# 			.stop(paste("SImageData have different element names:",
# 				paste(ls(x@data), collapse=" "),
# 				paste(ls(y@data), collapse=" "), sep="\n\t"))
# 		if ( prod(dim(y)) == 0 )
# 			return(x)
# 		if ( prod(dim(x)) == 0 )
# 			return(y)
# 		xdim <- x@dimnames
# 		ydim <- y@dimnames
# 		if ( any(sapply(xdim, is.null)) || any(sapply(ydim, is.null)) )
# 			.stop("SImageData elements must have dimnames for 'combine'")
# 		sharedRows <- intersect(xdim[[1]], ydim[[1]])
# 		sharedCols <- intersect(xdim[[2]], ydim[[2]])
# 		unionRows <- union(xdim[[1]], ydim[[1]])
# 		unionCols <- union(xdim[[2]], ydim[[2]])
# 		unionRowIds <- seq_along(unionRows)
# 		names(unionRowIds) <- unionRows
# 		unionColIds <- seq_along(unionCols)
# 		names(unionColIds) <- unionCols
# 		data <- new.env(parent=emptyenv())
# 		for ( nm in ls(x@data) ) {
# 			ok <- all.equal(x[[nm]][xdim[[1]] %in% sharedRows, xdim[[2]] %in% sharedCols],
# 				y[[nm]][ydim[[1]] %in% sharedRows, ydim[[2]] %in% sharedCols])
# 			if (!isTRUE(ok))
# 				.stop("SImageData element ", nm, " shared row and column elements differ: ", ok)
# 			data[[nm]] <- new(class(x[[nm]]), nrow=length(unionRows), ncol=length(unionCols))
# 			data[[nm]][unionRowIds[xdim[[1]]], unionColIds[xdim[[2]]]] <- x[[nm]]
# 			data[[nm]][unionRowIds[ydim[[1]]], unionColIds[ydim[[2]]]] <- y[[nm]]
# 		}
# 		if ( "sample" %in% union(names(x@coord), names(y@coord)) ) {
# 			samples <- union(levels(x@coord[["sample"]]), levels(y@coord[["sample"]]))
# 			x@coord[["sample"]] <- factor(as.character(x@coord[["sample"]]), levels=samples)
# 			y@coord[["sample"]] <- factor(as.character(y@coord[["sample"]]), levels=samples)
# 		}
# 		coord <- combine(x@coord, y@coord)
# 		positionArray <- generatePositionArray(coord)
# 		new(class(x),
# 			data=data,
# 			coord=coord,
# 			positionArray=positionArray,
# 			storageMode=x@storageMode,
# 			dim=dim(positionArray),
# 			dimnames=list(unionRows, unionCols))
# 	})
