
setMethod("initialize", "SImageData",
	function(.Object,
		positionArray = array(dim=c(x=0, y=0)),
		dim = c(0, 0),
		dimnames = list(NULL, NULL),
		...)
	{
		.Object@positionArray <- positionArray
		.Object@dim <- dim
		.Object@dimnames <- dimnames
		callNextMethod(.Object, ...)
	})

SImageData <- function(
	data = Hashmat(nrow=0, ncol=0),
	coord = expand.grid(
		x = seq_len(ncol(data)),
		y = seq_len(ifelse(ncol(data) > 0, 1, 0))),
	storageMode = c("immutableEnvironment",
		"lockedEnvironment", "environment"),
	positionArray = generatePositionArray(coord),
	...)
{
	storageMode <- match.arg(storageMode)
	if ( length(dim(data)) > 2 ) {
		dims <- dim(data)[-1]
		if ( is.null(names(dims)) || any(nchar(dims) == 0) ) {
			if ( length(dims) %in% c(2,3) ) {
				names(dims) <- c("x", "y", "z")[seq_along(dims)]
			} else {
				names(dims) <- paste("dim", seq_along(dims), sep="")
			}
		}
		positionArray <- array(seq_len(prod(dims)), dim=dims)
		dim(data) <- c(dim(data)[1], prod(dims))
	}
	if ( is.null(dim(data)) ) {
		dim <- c(0,0)
	} else {
		dim <- dim(data)
	}
	if ( is.null(dimnames(data)) ) {
		dimnames <- list(NULL, NULL)
	} else {
		dimnames <- dimnames(data)
	}
	.SImageData(iData=data,
		storageMode=storageMode,
		positionArray=positionArray,
		dim=dim,
		dimnames=dimnames,
		...)
}

setMethod("iData", "SImageData",
	function(object) object[["iData"]])

setReplaceMethod("iData", "SImageData",
	function(object, value) {
		object[["iData"]] <- value
		if ( validObject(object) )
			object			
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
		object@dimnames[[1]] <- value
		object
	})

setMethod("pixelNames", "SImageData",
	function(object) object@dimnames[[2]])

setReplaceMethod("pixelNames", "SImageData",
	function(object, value) {
		object@dimnames[[2]] <- value
		object
	})

setMethod("dim", "SImageData",
	function(x) c(Features=x@dim[[1]], dim(x@positionArray)))

setMethod("dims", "SImageData",
	function(object) {
		names <- ls(object@data)
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
			dims
		} else {
			matrix(nrow=0, ncol=0)
		}
	})

setMethod("[", "SImageData", function(x, i, j, ..., drop) {
	nargs <- nargs() - 1 - !missing(drop)
	if ( nargs != length(dim(x)) && !(nargs == 1 && missing(i)) )
		stop("incorrect number of dimensions")
	if ( missing(drop) ) drop <- TRUE
	args <- lapply(dim(x), function(dm) seq_len(dm))
	if ( !missing(i) ) args[[1]] <- i
	if ( !missing(j) ) args[[2]] <- j
	if ( nargs > 2 ) {
		dotargs <- as.list(match.call(expand.dots=FALSE))[["..."]]
		nonmissing <- !sapply(dotargs, is.symbol)
		if ( sum(nonmissing) > 0 )
			args[c(FALSE,FALSE,nonmissing)] <- dotargs[nonmissing]
	}
	inds <- do.call("[", c(list(x@positionArray), args[-1], drop=FALSE))
	cube <- matrix(NA, nrow=length(args[[1]]), ncol=length(inds))
	cube[,!is.na(inds)] <- x[["iData"]][args[[1]],inds[!is.na(inds)],drop=FALSE]
	# cube <- x[["iData"]][args[[1]],inds,drop=FALSE]
	dim(cube) <- c(dim(cube)[1], dim(inds))
	names(dim(cube)) <- c("Features", names(dim(x@positionArray)))
	if ( drop && all(dim(cube) == 1) )
		cube <- as.vector(cube)
	if ( drop && any(dim(cube) == 1) )
		dim(cube) <- dim(cube)[dim(cube) != 1]
	if ( drop && length(dim(cube)) == 1 )
		cube <- as.vector(cube)
	cube
})

.subset.SImageData <- function(x, i, j) {
	names <- ls(x@data)
	for ( nm in names ) {
		x[[nm]] <- x[[nm]][i,j,drop=FALSE]
	}
	x@dim <- c(length(i), length(j))
	x@dimnames <- list(x@dimnames[[1]][i], x@dimnames[[2]][j])
	x
}

