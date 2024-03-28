
#### Spatially-aware k-means ####
## ------------------------------

setMethod("spatialKMeans", "ANY",
	function(x, coord, r = 1, k = 2, ncomp = max(k),
		weights = c("gaussian", "adaptive"),
		neighbors = findNeighbors(coord, r=r),
		transpose = FALSE, niter = 2L,
		nchunks = getCardinalNChunks(),
		verbose = getCardinalVerbose(),
		BPPARAM = getCardinalBPPARAM(), ...)
{
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
				weights="adaptive", byrow=!transpose,
				nchunks=nchunks, verbose=verbose,
				BPPARAM=BPPARAM, ...)
			wts <- Map("*", wts, awts)
		}
	} else {
		wts <- rep_len(weights, length(neighbors))
		weights <- "user-provided weights"
	}
	proj <- spatialFastmap(x, r=r, ncomp=ncomp,
		neighbors=neighbors, weights=wts,
		transpose=transpose, niter=niter,
		nchunks=nchunks, verbose=verbose,
		BPPARAM=BPPARAM, ...)
	k <- rev(sort(k))
	ans <- vector("list", length=length(k))
	for ( i in seq_along(k) )
	{
		if ( i > 1L ) {
			centers <- ans[[i - 1L]]$centers[seq_len(k[i]),,drop=FALSE]
		} else {
			centers <- k[i]
		}
		if ( verbose )
			message("fitting k-means for  k = ", k[i])
		ans[[i]] <- kmeans(proj$x, centers=centers, ...)
		ans[[i]]$weights <- weights
		ans[[i]]$ncomp <- ncomp
		ans[[i]]$r <- r
		ans[[i]]$k <- k[i]
	}
	if ( verbose )
		message("returning spatial k-means")
	if ( length(ans) > 1L ) {
		mcols <- DataFrame(r=r, k=k, weights=weights, ncomp=ncomp)
		ResultsList(ans, mcols=mcols)
	} else {
		ans[[1L]]
	}
})

setMethod("spatialKMeans", "SpectralImagingExperiment", 
	function(x, r = 1, k = 2, ncomp = max(k),
		weights = c("gaussian", "adaptive"),
		neighbors = findNeighbors(x, r=r), ...)
{
	if ( length(processingData(x)) > 0L )
		warning("pending processing steps will be ignored")
	ans <- spatialKMeans(spectra(x),
		coord=coord(x), r=r, k=k, ncomp=ncomp,
		neighbors=neighbors, weights=weights, transpose=TRUE, ...)
	if ( is(ans, "ResultsList") ) {
		f <- function(a) as(SpatialResults(a, x), "SpatialKMeans")
		ResultsList(lapply(ans, f), mcols=mcols(ans))
	} else {
		as(SpatialResults(ans, x), "SpatialKMeans")
	}
})

