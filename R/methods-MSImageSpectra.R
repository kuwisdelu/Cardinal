
setMethod("initialize",
	signature(.Object = "MSImageSpectra"),
	function(.Object,
			positionArray,
			...) {
		.Object@positionArray <- positionArray
		callNextMethod(.Object,
			...)
	})

MSImageSpectra <- function(spectra, coord,
	storageMode = "immutableEnvironment",
	positionArray = if ( !missing(coord) ) generatePositionArray(coord) else NULL,
	...)
{
	if ( !is.array(spectra) || dim(spectra) < 2 ) stop("spectra must be an array or matrix")
	if ( length(dim(spectra)) > 2 ) {
		if ( !missing(coord) || !is.null(positionArray) ) 
			warning("spectra is a datacube; ignoring user-provided coord")
		positionArray <- array(seq_len(prod(dim(spectra)[-1])), dim=dim(spectra)[-1])
		dim(spectra) <- c(dim(spectra)[1], prod(dim(spectra)[-1]))
	}
	.MSImageSpectra(spectra=spectra,
		storageMode=storageMode,
		positionArray=positionArray,
		...)
}

setMethod("dim", "MSImageSpectra",
	function(x) {
		c(Features=nrow(x@data[["spectra"]]), dim(x@positionArray))
	})

setMethod("[", "MSImageSpectra", function(x, i, j, ..., drop) {
	nargs <- nargs() - 1 - !missing(drop)
	if ( nargs != length(dim(x)) + 1 && !(nargs == 1 && missing(i)) )
		stop("incorrect number of dimensions")
	if ( missing(drop) ) drop <- TRUE
	args <- lapply(dim(x), function(dm) seq_len(dm))
	args <- c(list(1:nrow(x@data[["spectra"]])), args)
	if ( !missing(i) ) args[[1]] <- i
	if ( !missing(j) ) args[[2]] <- j
	if ( nargs > 2 ) {
		dotargs <- as.list(match.call(expand.dots=FALSE))[["..."]]
		nonmissing <- !sapply(dotargs, is.symbol)
		if ( sum(nonmissing) > 0 )
			args[c(FALSE,FALSE,nonmissing)] <- dotargs[nonmissing]
	}
	ids <- do.call("[", c(list(x@positionArray), args[-1], drop=FALSE))
	arr <- x@data[["spectra"]][args[[1]],ids,drop=FALSE]
	dim(arr) <- c(dim(arr)[1], dim(ids))
	if ( drop && all(dim(arr) == 1) )
		arr <- as.vector(arr)
	if ( drop && any(dim(arr) == 1) )
		dim(arr) <- dim(arr)[dim(arr) != 1]
	if ( drop && length(dim(arr)) == 1 )
		arr <- as.vector(arr)
	arr
})
