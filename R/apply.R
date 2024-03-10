
setMethod("featureApply", "SpectralImagingExperiment",
	function(.object, .fun, ..., .simplify = TRUE, .outpath = NULL,
		.blocks = getCardinalNumBlocks(), .verbose = getCardinalVerbose(),
		BPPARAM = getCardinalBPPARAM())
	{
		.Deprecated("chunkApply")
		.checkForIncompleteProcessing(.object)
		chunkApply(spectra(.object), FUN=.fun, MARGIN=1L, ...,
			simplify=.simplify, nchunks=.blocks,
			outpath=.outpath, verbose=.verbose,
			BPPARAM=BPPARAM)
	})

setMethod("pixelApply", "SpectralImagingExperiment",
	function(.object, .fun, ..., .simplify = TRUE, .outpath = NULL,
		.blocks = getCardinalNumBlocks(), .verbose = getCardinalVerbose(),
		BPPARAM = getCardinalBPPARAM())
	{
		.Deprecated("chunkApply")
		chunkApply(spectra(.object), FUN=.fun, MARGIN=2L, ...,
			simplify=.simplify, nchunks=.blocks,
			outpath=.outpath, verbose=.verbose,
			BPPARAM=BPPARAM)
	})

setMethod("spatialApply", "SpectralImagingExperiment",
	function(.object, .r, .fun, ..., .dist = "chebyshev",
		.simplify = TRUE, .outpath = NULL,
		.blocks = getCardinalNumBlocks(),
		.verbose = getCardinalVerbose(),
		BPPARAM = getCardinalBPPARAM())
	{
		.Defunct("chunkApply")
	})
