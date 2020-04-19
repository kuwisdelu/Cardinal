
setMethod("spatialApply", "SparseImagingExperiment",
	function(.object, .r, .fun, ...,
			.dist = "chebyshev",
			.simplify = TRUE,
			.outpath = NULL,
			.params = list(),
			.blocks = getCardinalNumBlocks(),
			.verbose = getCardinalVerbose(),
			.view = "element",
			BPREDO = list(),
			BPPARAM = getCardinalBPPARAM())
	{
		.checkForIncompleteProcessing(.object)
		nb <- findNeighbors(.object, r=.r, dist=.dist, offsets=TRUE)
		idx <- seq_len(ncol(.object))
		alist <- list(offsets=attr(nb, "offsets"),
					neighbors=c(nb), idx=idx)
		alist <- c(alist, .params)
		FUN <- function(x, ...) {
			if ( !is.null(attr(x, "chunk_id")) ) {
				# view = "chunk"
				i <- attr(x, "chunk_elt")
				ci <- attr(x, "pattern_id") # == $idx
				nb <- attr(x, "pattern_elt") # == $neighbors
				ni <- lapply(nb, match, i)
				attr(x, "centers") <- match(ci, i)
				attr(x, "neighbors") <- ni
			} else {
				# view = "element"
				i <- attr(x, "idx")
				nb <- attr(x, "neighbors")
				attr(x, "centers") <- match(i, nb)
				attr(x, "neighbors") <- seq_len(ncol(x))
			}
			.fun(x, ...)
		}
		ans <- chunk_apply(iData(.object),
			FUN=FUN,
			MARGIN=2L,
			...,
			simplify=.simplify,
			chunks=.blocks,
			view=.view,
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
