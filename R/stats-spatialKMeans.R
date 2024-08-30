
#### Spatially-aware k-means ####
## ------------------------------

setMethod("spatialKMeans", "ANY",
	function(x, coord, r = 1, k = 2, ncomp = max(k),
		weights = c("gaussian", "adaptive"),
		neighbors = findNeighbors(coord, r=r),
		transpose = TRUE, niter = 10L,
		centers = TRUE, correlation = TRUE,
		verbose = getCardinalVerbose(), chunkopts = list(),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( "method" %in% ...names() ) {
		.Deprecated(old="method", new="weights")
		weights <- list(...)$method
	}
	if ( is.character(weights) ) {
		weights <- match.arg(weights)
		.Log("computing ", weights, " weights",
			message=verbose)
		nbwts <- spatialWeights(x=x,
			coord=coord, r=r, byrow=!transpose,
			weights=weights, neighbors=neighbors,
			verbose=verbose, chunkopts=chunkopts,
			BPPARAM=BPPARAM)
	} else {
		.Log("using custom weights",
			message=verbose)
		nbwts <- rep_len(weights, length(neighbors))
		weights <- "custom"
	}
	if ( transpose ) {
		k <- pmin(k, nrow(x))
	} else {
		k <- pmin(k, ncol(x))
	}
	proj <- spatialFastmap(x, r=r, ncomp=ncomp,
		neighbors=neighbors, weights=nbwts,
		transpose=transpose, niter=niter,
		verbose=verbose, chunkopts=chunkopts,
		BPPARAM=BPPARAM, ...)
	k <- rev(sort(k))
	ans <- vector("list", length=length(k))
	truecenters <- vector("list", length=length(k))
	for ( i in seq_along(k) )
	{
		if ( i > 1L ) {
			icenters <- ans[[i - 1L]]$centers[seq_len(k[i]),,drop=FALSE]
		} else {
			icenters <- k[i]
		}
		.Log("fitting k-means for k = ", k[i],
			message=verbose)
		ans[[i]] <- kmeans(proj$x, centers=icenters, ...)
		if ( centers )
		{
			.Log("calculating cluster centers",
				message=verbose)
			if ( transpose ) {
				truecenters[[i]] <- rowStats(x, "mean",
					group=as.factor(ans[[i]]$cluster),
					verbose=verbose, chunkopts=chunkopts,
					BPPARAM=BPPARAM)
			} else {
				truecenters[[i]] <- colStats(x, "mean",
					group=as.factor(ans[[i]]$cluster),
					verbose=verbose, chunkopts=chunkopts,
					BPPARAM=BPPARAM)
			}
		}
		if ( correlation )
		{
			.Log("calculating spatial correlations with clusters",
				message=verbose)
			cls <- as.factor(ans[[i]]$cluster)
			lvl <- setNames(levels(cls), levels(cls))
			mask <- lapply(lvl, function(ci) cls %in% ci)
			margin <- if (transpose) 1L else 2L
			FUN <- isofun(function(xi, mask) {
				vapply(mask, stats::cor, numeric(1L), y=xi)
			}, CardinalEnv())
			corr <- chunkApply(x, margin, FUN, mask=mask,
				verbose=verbose, chunkopts=chunkopts,
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
	for ( i in seq_along(k) )
	{
		if ( centers ) {
			ans[[i]]$centers <- truecenters[[i]]
		} else {
			ans[[i]]$centers <- NULL
		}
	}
	names(ans) <- paste0("k=", k)
	.Log("returning spatial k-means",
		message=verbose)
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
		.Warn("pending processing steps will be ignored")
	ans <- spatialKMeans(spectra(x),
		coord=coord(x), r=r, k=k, ncomp=ncomp,
		neighbors=neighbors, weights=weights,
		transpose=TRUE, ...)
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
		.Error("missing component: 'correlation'")
	cluster <- rep(colnames(corr), each=nrow(corr))
	topf <- DataFrame(cluster=cluster, correlation=as.vector(corr))
	topf <- .rank_featureData(object, topf, sort.by)
	head(topf, n=n)
})

setMethod("plot", c(x = "SpatialKMeans", y = "missing"),
	function(x, type = c("correlation", "centers"), ..., xlab, ylab)
{
	type <- match.arg(type)
	if ( missing(xlab) )
		xlab <- NULL
	if ( type == "correlation" ) {
		if ( is.null(x$correlation) )
			.Error("missing component: 'correlation'")
		if ( missing(ylab) )
			ylab <- "Correlation"
		callNextMethod(x, y=x$correlation, xlab=xlab, ylab=ylab, ...)
	} else {
		if ( is.null(x$centers) )
			.Error("missing component: 'centers'")
		if ( missing(ylab) )
			ylab <- "Centers"
		callNextMethod(x, y=x$centers, xlab=xlab, ylab=ylab, ...)
	}
})

setMethod("image", c(x = "SpatialKMeans"),
	function(x, type = "cluster", ...)
{
	type <- match.arg(type)
	callNextMethod(x, y=as.factor(x$cluster), ...)
})


