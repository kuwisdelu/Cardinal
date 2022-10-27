
setMethod("spatialApply", "SparseImagingExperiment",
	function(.object, .r, .fun, ..., .dist = "chebyshev",
		.simplify = TRUE, .outpath = NULL,
		.blocks = getCardinalNumBlocks(),
		.verbose = getCardinalVerbose(),
		BPPARAM = getCardinalBPPARAM())
	{
		.Deprecated("chunkApply")
		.checkForIncompleteProcessing(.object)
		nb <- findNeighbors(.object, r=.r, dist=.dist, offsets=TRUE)
		ans <- chunkApply(iData(.object), FUN=.fun, MARGIN=2L, ...,
			simplify=.simplify, nchunks=.blocks,
			outpath=.outpath, verbose=.verbose,
			depends=nb, BPPARAM=BPPARAM)
		attr(ans, "neighbors") <- nb
		ans
	})
