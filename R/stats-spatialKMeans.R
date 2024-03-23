
#### Spatially-aware K-Means ####
## ------------------------------

setMethod("spatialKMeans", "ANY",
	function(x, coord, r = 1, k = 3, ncomp = 20,
		neighbors = findNeighbors(coord, r=r),
		weights = c("gaussian", "adaptive"),
		transpose = FALSE, niter = 2L,
		BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( is.character(weights) ) {
		if ( getCardinalVerbose() )
			message("calculating gaussian weights")
		wts <- spatialWeights(as.matrix(coord), neighbors=neighbors)
		if ( match.arg(weights) == "adaptive" )
		{
			if ( getCardinalVerbose() )
				message("calculating adaptive weights")
			awts <- spatialWeights(x, neighbors=neighbors,
				weights="adaptive", byrow=!transpose,
				nchunks=getCardinalNChunks(),
				verbose=getCardinalVerbose(),
				BPPARAM=BPPARAM, ...)
			wts <- Map("*", wts, awts)
		}
	} else {
		wts <- rep_len(weights, length(neighbors))
		weights <- "custom"
	}
	proj <- spatialFastmap(x, r=r, ncomp=ncomp,
		neighbors=neighbors, weights=wts,
		transpose=transpose, niter=niter,
		BPPARAM=BPPARAM)
	k <- rev(sort(k))
	ans <- vector("list", length=length(k))
	for ( i in seq_along(k) )
	{
		if ( i > 1L ) {
			centers <- ans[[i - 1L]]$centers[seq_len(k[i]),,drop=FALSE]
		} else {
			centers <- k[i]
		}
		if ( getCardinalVerbose() )
			message("fitting k-means for  k = ", k[i])
		ans[[i]] <- kmeans(proj$x, centers=centers, ...)
		ans[[i]]$weights <- weights
		ans[[i]]$ncomp <- ncomp
		ans[[i]]$r <- r
	}
	if ( getCardinalVerbose() )
		message("returning spatially-aware k-means")
	if ( length(ans) > 1L ) {
		params <- DataFrame(r=r, k=k, weights=weights, ncomp=ncomp)
		ResultsList(ans, mcols=params)
	} else {
		ans[[1L]]
	}
})

setMethod("spatialKMeans", "SpectralImagingExperiment", 
	function(x, r = 1, k = 3, ncomp = 20,
		neighbors = findNeighbors(x, r=r),
		weights = c("gaussian", "adaptive"),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( length(processingData(x)) > 0L )
		warning("pending processing steps will be ignored")
	ans <- spatialKMeans(spectra(x),
		coord=coord(x), r=r, k=k, ncomp=ncomp,
		neighbors=neighbors, weights=weights, transpose=TRUE,
		BPPARAM=BPPARAM, ...)
	if ( is(ans, "ResultsList") ) {
		FUN <- function(a) as(SpatialResults(a, x), "SpatialKMeans")
		ResultsList(lapply(ans, FUN), mcols=mcols(ans))
	} else {
		as(SpatialResults(ans, x), "SpatialKMeans")
	}
})

