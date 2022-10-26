
setMethod("featureApply", "SparseImagingExperiment",
	function(.object, .fun, ..., .simplify = TRUE, .outpath = NULL,
		.blocks = getCardinalNumBlocks(), .verbose = getCardinalVerbose(),
		BPPARAM = getCardinalBPPARAM())
	{
		.checkForIncompleteProcessing(.object)
		chunkApply(iData(.object), FUN=.fun, MARGIN=1L, ...,
			simplify=.simplify, nchunks=.blocks,
			outpath=.outpath, verbose=.verbose,
			BPPARAM=BPPARAM)
	})

