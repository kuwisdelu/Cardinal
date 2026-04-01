
#### Spectral pre-processing execution ####
## ----------------------------------------

setMethod("applyProcessing", "SpectralImagingArrays",
	function(object, f = processingChunkFactor(object),
		POSTFUN = relistFromTuples, POSTARGS = list(),
		verbose = getCardinalVerbose(), chunkopts = list(),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	.process_SpectralImagingArrays(object, f=f,
		POSTFUN=POSTFUN, POSTARGS=POSTARGS,
		verbose=verbose, chunkopts=chunkopts,
		BPPARAM=BPPARAM, ...)
})

.process_SpectralImagingArrays <- function(object,
	f = processingChunkFactor(object),
	POSTFUN = relistFromTuples, POSTARGS = list(),
	verbose = getCardinalVerbose(), chunkopts = list(),
	BPPARAM = getCardinalBPPARAM(), ...)
{
	stop("not implemented yet")
}

.process_ITER <- function(object,
	f = processingChunkFactor(object),
	verbose = getCardinalVerbose())
{

}

.process_FUN <- function(x, processingSteps, processingVariables)
{

}
