
setMethod("spatialApply", "SparseImagingExperiment",
	function(.object, .r, .fun, ...,
			.dist = "chebyshev",
			.blocks = getOption("Cardinal.numblocks"),
			.simplify = TRUE,
			.outpath = NULL,
			.params = list(),
			.verbose = getOption("Cardinal.verbose"),
			BPREDO = list(),
			BPPARAM = bpparam())
	{
		.checkForIncompleteProcessing(.object)
		nb <- findNeighbors(.object, r=.r, dist=.dist, offsets=TRUE)
		idx <- seq_len(ncol(.object))
		centers <- numeric(length(idx))
		for ( i in idx )
			centers[i] <- match(i, nb[[i]])
		alist <- list(
			idx=idx,
			center=centers,
			offsets=attr(nb, "offsets"))
		alist <- c(alist, .params)
		ans <- chunk_apply(iData(.object),
			FUN=.fun,
			MARGIN=2L,
			...,
			simplify=.simplify,
			chunks=.blocks,
			attr=list(mcols=fData(.object)),
			alist=alist,
			pattern=nb,
			outfile=.outpath,
			verbose=.verbose,
			BPREDO=BPREDO,
			BPPARAM=BPPARAM)
		attr(ans, "neighbors") <- nb
		ans
	})
