
#### Spectral pre-processing execution ####
## ----------------------------------------

setMethod("applyProcessing", "SpectralImagingArrays",
	function(object, ...,
		f = processingChunkFactor(object),
		REDUCE, init, reduce.in.order = TRUE,
		verbose = getCardinalVerbose(),
		BPPARAM = getCardinalBPPARAM())
{
	if ( missing(REDUCE) )
		REDUCE <- combine
	if ( missing(init) )
		init <- NULL
	.chunkapply_SpectralImagingArrays(object, ...,
		CHUNKFUN=.applyProcessing_SpectralImagingArrays,
		REDUCE=REDUCE, init=init, reduce.in.order=reduce.in.order,
		f=f, verbose=verbose, BPPARAM=BPPARAM)
})

.applyProcessing_SpectralImagingArrays <- function(object)
{
	result <- dropProcessing(object)
	X <- .list_SpectralImagingArrays(object, withProcessing=TRUE)
	spectraData(result) <- as(.zipdown(X), "SpectraArrays")
	result
}

.process_spectra_list <- function(X, queue, mcols)
{
	result <- vector("list", length=length(X))
	for ( i in seq_along(X) )
	{
		xi <- X[[i]]
		if ( length(mcols) > 0L ) {
			margs <- as.list(mcols[i,,drop=FALSE])
		} else {
			margs <- list()
		}
		for ( step in queue ) {
			step <- updateProcessingStep(step, margs)
			xi <- executeProcessingStep(step, xi)
			if ( !is.list(xi) || is.null(names(xi)) )
				stop("ProcessingStep FUN must return a named list")
		}
		result[[i]] <- xi
	}
	result
}

