
#### Spatially-aware Gaussian mixture models ####
## ---------------------------------------------

setMethod("spatialDGMM", "ANY",
	function(x, coord, i, r = 1, k = 2, groups = NULL,
		weights = c("gaussian", "adaptive"),
		neighbors = findNeighbors(coord, r=r, groups=groups),
		annealing = TRUE, compress = TRUE, byrow = FALSE,
		nchunks = getCardinalNChunks(),
		verbose = getCardinalVerbose(),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( "method" %in% ...names() ) {
		.Deprecated(old="method", new="weights")
		weights <- list(...)$method
	}
	if ( is.character(weights) ) {
		weights <- match.arg(weights)
		if ( verbose )
			message("calculating gaussian weights")
		wts <- spatialWeights(as.matrix(coord), neighbors=neighbors)
		if ( weights == "adaptive" )
		{
			if ( verbose )
				message("calculating adaptive weights")
			awts <- spatialWeights(x, neighbors=neighbors,
				weights="adaptive", byrow=!byrow,
				nchunks=nchunks, verbose=verbose,
				BPPARAM=BPPARAM, ...)
			wts <- Map("*", wts, awts)
		}
	} else {
		wts <- rep_len(weights, length(neighbors))
		weights <- "user-provided weights"
	}
	if ( !missing(i) && !is.null(i) ) {
		drop <- if (is.matter(x) || is.sparse(x)) NULL else FALSE
		if ( byrow ) {
			x <- x[i,,drop=drop]
		} else {
			x <- x[,i,drop=drop]
		}
	} else {
		i <- if (byrow) seq_len(nrow(x)) else seq_len(ncol(x))
	}
	k <- rev(sort(k))
	ans <- vector("list", length=length(k))
	for ( j in seq_along(k) )
	{
		if ( verbose ) {
			lab <- if (length(i) != 1L) "models" else "model"
			message("fitting spatial Gaussian mixture ",
				lab, " for k = ", k[j])
		}
		ans[[j]] <- sgmixn(NULL, NULL, x, r=r, k=k[j], group=groups,
			weights=wts, neighbors=neighbors, byrow=byrow,
			annealing=annealing, compress=compress,
			nchunks=nchunks, verbose=verbose,
			BPPARAM=BPPARAM, ...)
		ans[[j]]$weights <- weights
		ans[[j]]$r <- r
		ans[[j]]$k <- k[j]
	}
	if ( verbose ) {
		lab <- if (length(k) != 1L || length(i) != 1L) "models" else "model"
		message("returning spatial Gaussian mixture ", lab)
	}
	if ( length(ans) > 1L ) {
		ResultsList(ans,
			mcols=DataFrame(r=r, k=k, weights=weights))
	} else {
		ans[[1L]]
	}
})

setMethod("spatialDGMM", "SpectralImagingExperiment", 
	function(x, i, r = 1, k = 2, groups = run(x),
		weights = c("gaussian", "adaptive"),
		neighbors = findNeighbors(coord(x), r=r, groups=groups), ...)
{
	if ( length(processingData(x)) > 0L )
		warning("pending processing steps will be ignored")
	ans <- spatialDGMM(spectra(x),
		coord=coord(x), i=i, r=r, k=k, groups=groups,
		neighbors=neighbors, weights=weights, byrow=TRUE, ...)
	if ( missing(i) || is.null(i) ) {
		f <- function(a) as(SpatialResults(a, x), "SpatialDGMM")
	} else {
		f <- function(a) as(SpatialResults(a, x,
			featureData=featureData(x)[i,,drop=FALSE],
			pixelData=pixelData(x)), "SpatialDGMM")
	}
	if ( is(ans, "ResultsList") ) {
		ResultsList(lapply(ans, f), mcols=mcols(ans))
	} else {
		f(ans)
	}
})

