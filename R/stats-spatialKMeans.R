
#### Spatially-aware k-means ####
## ------------------------------

setMethod("spatialKMeans", "ANY",
	function(x, coord, r = 1, k = 2, ncomp = max(k),
		weights = c("gaussian", "adaptive"),
		neighbors = findNeighbors(coord, r=r),
		transpose = FALSE, niter = 2L, correlation = TRUE,
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
				weights="adaptive", byrow=!transpose,
				nchunks=nchunks, verbose=verbose,
				BPPARAM=BPPARAM, ...)
			wts <- Map("*", wts, awts)
		}
	} else {
		wts <- rep_len(weights, length(neighbors))
		weights <- "user-provided weights"
	}
	if ( transpose ) {
		k <- pmin(k, nrow(x))
	} else {
		k <- pmin(k, ncol(x))
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
			message("fitting k-means for k = ", k[i])
		ans[[i]] <- kmeans(proj$x, centers=centers, ...)
		if ( correlation )
		{
			if ( verbose )
				message("calculating correlations with clusters")
			cls <- as.factor(ans[[i]]$cluster)
			lvl <- setNames(levels(cls), levels(cls))
			cls <- lapply(lvl, function(ci) cls %in% ci)
			margin <- if (transpose) 1L else 2L
			FUN <- function(xi) vapply(cls, cor, numeric(1L), y=xi)
			corr <- chunkApply(x, margin, FUN,
				nchunks=nchunks, verbose=verbose,
				BPPARAM=BPPARAM)
			corr <- do.call(rbind, corr)
			rownames(corr) <- dimnames(x)[[margin]]
			ans[[i]]$correlation <- corr
		}
		ans[[i]]$weights <- weights
		ans[[i]]$ncomp <- ncomp
		ans[[i]]$r <- r
		ans[[i]]$k <- k[i]
	}
	names(ans) <- paste0("k=", k)
	if ( verbose )
		message("returning spatial k-means")
	if ( length(ans) > 1L ) {
		ResultsList(ans,
			mcols=DataFrame(r=r, k=k, weights=weights, ncomp=ncomp))
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
	f <- function(a) as(SpatialResults(a, x), "SpatialKMeans")
	if ( is(ans, "ResultsList") ) {
		ResultsList(lapply(ans, f), mcols=mcols(ans))
	} else {
		f(ans)
	}
})

setMethod("topFeatures", "SpatialKMeans",
	function(object, n = Inf, sort.by = "correlation", ...)
{
	sort.by <- match.arg(sort.by)
	corr <- object$correlation
	if ( is.null(corr) )
		stop("missing component: 'correlation'")
	cluster <- rep(colnames(corr), each=nrow(corr))
	topf <- DataFrame(cluster=cluster, correlation=as.vector(corr))
	topf <- .rank_featureData(object, topf, sort.by)
	head(topf, n=n)
})


