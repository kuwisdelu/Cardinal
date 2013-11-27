
setMethod("initialize",
	signature(.Object = "MSImageSpectra"),
	function(.Object,
			positionArray = array(integer(0), dim=0),
			...) {
		.Object@positionArray <- positionArray
		callNextMethod(.Object,
			...)
	})

MSImageSpectra <- function(spectra, coord,
	storageMode = c("immutableEnvironment", "lockedEnvironment", "environment"),
	positionArray = if ( !missing(coord) ) generatePositionArray(coord) else NULL,
	...)
{
	storageMode <- match.arg(storageMode)
	if ( missing(spectra) ) spectra <- matrix(nrow=0, ncol=0)
	if ( length(dim(spectra)) < 2 || !is.array(spectra) ) stop("spectra must be an array or matrix")
	if ( length(dim(spectra)) > 2 || is.null(positionArray) ) {
		if ( !missing(coord) ) warning("spectra is a datacube; ignoring user-provided coord")
		positionArray <- array(seq_len(prod(dim(spectra)[-1])), dim=dim(spectra)[-1])
		dim(spectra) <- c(dim(spectra)[1], prod(dim(spectra)[-1]))
	}
	.MSImageSpectra(spectra=spectra,
		storageMode=storageMode,
		positionArray=positionArray,
		...)
}

setMethod("spectra", "MSImageSpectra",
	function(object) object[["spectra"]])

setReplaceMethod("spectra", "MSImageSpectra",
	function(object, value) {
		object[["spectra"]] <- value
		if ( validObject(object) )
			object			
	})

setMethod("dim", "MSImageSpectra",
	function(x) c(Features=nrow(x[["spectra"]]), dim(x@positionArray)))

setMethod("dims", "MSImageSpectra",
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
			NULL
		}
	})

setMethod("combine", signature = signature(x = "MSImageSpectra", y = "MSImageSpectra"),
	function(x, y, ...) {
		if ( any(dim(x)[[1]] != dim(y)[[1]]) )
			stop(paste("MSImageSpectra have different numbers of features:",
				paste(dim(x)[[1]], collapse=" "),
				paste(dim(x)[[1]], collapse=" "), sep="\n\t"))
		object <- callNextMethod(x, y, ...)
		offset <- as.integer(prod(dim(x)[-1]))
		along <- length(dim(x@positionArray)) + 1
		new("MSImageSpectra", data=object@data, storageMode=object@storageMode,
			positionArray=abind(x@positionArray, y@positionArray + offset, along=along))
	})

setMethod("[", "MSImageSpectra", function(x, i, j, ..., drop) {
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
	ids <- do.call("[", c(list(x@positionArray), args[-1], drop=FALSE))
	arr <- x[["spectra"]][args[[1]],ids,drop=FALSE]
	dim(arr) <- c(dim(arr)[1], dim(ids))
	if ( drop && all(dim(arr) == 1) )
		arr <- as.vector(arr)
	if ( drop && any(dim(arr) == 1) )
		dim(arr) <- dim(arr)[dim(arr) != 1]
	if ( drop && length(dim(arr)) == 1 )
		arr <- as.vector(arr)
	arr
})
