
setMethod("initialize", "SImageData",
	function(.Object, positionArray = array(dim=c(x=0, y=0)), ...) {
		.Object@positionArray <- positionArray
		.Object <- callNextMethod(.Object, ...)
		# the following is a hack to prevent copying when assigning 'dimnames'
		# see 'pixelNames' and 'featureNames' for details
		evalq(.value <- NULL, envir=.Object@data)
		.Object
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
	.SImageData(data0=data,
		storageMode=storageMode,
		positionArray=positionArray,
		...)
}

setMethod("iData", "SImageData",
	function(object) object[["data0"]])

setReplaceMethod("iData", "SImageData",
	function(object, value) {
		object[["data0"]] <- value
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
	function(object) dimnames(object[["data0"]])[[1]])

setReplaceMethod("featureNames", "SImageData",
	function(object, value) {
		object@data$.value <- value
		# use of evalq() prevents unnecessary copying of data0
		evalq(dimnames(data0)[[1]] <- .value, envir=object@data)
		evalq(.value <- NULL, envir=object@data)
		object
	})

setMethod("pixelNames", "SImageData",
	function(object) dimnames(object[["data0"]])[[2]])

setReplaceMethod("pixelNames", "SImageData",
	function(object, value) {
		object@data$.value <- value
		# use of evalq() prevents unnecessary copying of data0
		evalq(dimnames(data0)[[2]] <- .value, envir=object@data)
		evalq(.value <- NULL, envir=object@data)
		object
	})

setMethod("dim", "SImageData",
	function(x) c(Features=nrow(x[["data0"]]), dim(x@positionArray)))

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
	cube[,!is.na(inds)] <- x[["data0"]][args[[1]],inds[!is.na(inds)],drop=FALSE]
	# cube <- x[["data0"]][args[[1]],inds,drop=FALSE]
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
