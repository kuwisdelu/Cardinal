
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
		alist <- list(
			idx=idx,
			neighbors=c(nb),
			offsets=attr(nb, "offsets"))
		alist <- c(alist, .params)
		FUN <- function(x, ...) {
			if ( is.null(attr(x, "chunk_id")) ) {
				# view = "element"
				i <- attr(x, "idx")
				nb <- attr(x, "neighbors")
				attr(x, "centers") <- match(i, nb)
				attr(x, "neighbors") <- seq_len(ncol(x))
			} else {
				# view = "chunk"
				i <- attr(x, "chunk_elt")
				ci <- attr(x, "idx")
				nb <- attr(x, "neighbors")
				ni <- lapply(nb, match, i)
				attr(x, "centers") <- match(ci, i)
				attr(x, "neighbors") <- ni
				attr(x, "pattern_id") <- NULL
				attr(x, "pattern_elt") <- NULL
				attr(x, "chunk_id") <- NULL
				attr(x, "chunk_elt") <- NULL
			}
			.fun(x, ...)
		}
		ans <- chunk_apply(iData(.object),
			FUN=FUN,
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
