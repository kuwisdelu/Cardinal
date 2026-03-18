
#### Spectral pre-processing ####
## ------------------------------

setMethod("addProcessing", "SpectralImagingArrays",
	function(object, FUN, ...,
		label = NULL, spectraVariables = character())
{
	ps <- ProcessingStep(FUN, ARGS=list(...))
	ps <- setNames(list(ps), label)
	psvars <- union(object@processingVariables, spectraVariables)
	object@processing <- c(object@processing, ps)
	object@processingVariables <- psvars
	if ( validObject(object) )
		object
})

dropProcessing <- function(object, ...)
{
	object@processing <- list()
	object@processingVariables <- character()
	if ( validObject(object) )
		object
}

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

