
#### Spectral pre-processing execution ####
## ----------------------------------------

.process_SpectralImagingArrays <- function(object,
	f = processingChunkFactor(object),
	ZIPFUN = relistFromTuples, ZIPARGS = list(),
	verbose = getCardinalVerbose(), chunkopts = list(),
	BPPARAM = getCardinalBPPARAM(), ...)
{
	stop("not implemented yet")
}

setMethod("applyProcessing", "SpectralImagingArrays",
	function(object, f = processingChunkFactor(object),
		ZIPFUN = relistFromTuples, ZIPARGS = list(),
		verbose = getCardinalVerbose(), chunkopts = list(),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	.process_SpectralImagingArrays(object, f=f,
		ZIPFUN=ZIPFUN, ZIPARGS=ZIPARGS,
		verbose=verbose, chunkopts=chunkopts,
		BPPARAM=BPPARAM, ...)
})

