
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

setMethod("pixelApply", "SparseImagingExperiment",
	function(.object, .fun, ..., .simplify = TRUE, .outpath = NULL,
		.blocks = getCardinalNumBlocks(), .verbose = getCardinalVerbose(),
		BPPARAM = getCardinalBPPARAM())
	{
		.checkForIncompleteProcessing(.object)
		chunkApply(iData(.object), FUN=.fun, MARGIN=2L, ...,
			simplify=.simplify, nchunks=.blocks,
			outpath=.outpath, verbose=.verbose,
			BPPARAM=BPPARAM)
	})

setMethod("spatialApply", "SparseImagingExperiment",
	function(.object, .r, .fun, ..., .dist = "chebyshev",
		.simplify = TRUE, .outpath = NULL,
		.blocks = getCardinalNumBlocks(),
		.verbose = getCardinalVerbose(),
		BPPARAM = getCardinalBPPARAM())
	{
		.Deprecated(msg="'spatialApply' is deprecated.\nSee help('Deprecated')")
		.checkForIncompleteProcessing(.object)
		nb <- findNeighbors(.object, r=.r, dist=.dist, offsets=TRUE)
		ans <- chunkApply(iData(.object), FUN=.fun, MARGIN=2L, ...,
			simplify=.simplify, nchunks=.blocks,
			outpath=.outpath, verbose=.verbose,
			depends=nb, BPPARAM=BPPARAM)
		attr(ans, "neighbors") <- nb
		ans
	})
