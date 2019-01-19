
setMethod("spatialKMeans", "SparseImagingExperiment",
	function(x, r = 1, k = 2,
		method = c("gaussian", "adaptive"),
		dist = "chebyshev", tol.dist = 1e-9,
		seed = 1, iter.max = 10, nstart = 1,
		algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"),
		ncomp = 10, BPPARAM = bpparam(), ...)
	{
		.checkForIncompleteProcessing(x)
		BPPARAM <- .protectNestedBPPARAM(BPPARAM)
		method <- match.arg(method)
		if ( max(k) > ncol(x) )
			.stop("can't fit more clusters than number of pixels")
		if ( max(k) > nrow(x) )
			.stop("can't fit more clusters than number of features")
		ncomp <- min(ncomp, nrow(x))
		.message("reducing dimension prior to k-means")
		fastmap <- spatialFastmap(x, r=r, ncomp=ncomp, method=method,
			dist=dist, tol.dist=tol.dist, iter.max=1, BPPARAM=BPPARAM)
		results <- list()
		.message("clustering components...")
		for ( i in r ) {
			results[[i]] <- bplapply(k, function(ki, BPPARAM) {
				.message("r = ", r[i], ", k = ", ki)
				.spatialKMeans2(x=x, r=r[i], k=ki, fastmap=fastmap, seed=seed,
					iter.max=iter.max, nstart=nstart, algorithm=algorithm,
					BPPARAM=BPPARAM, ...)
			}, BPPARAM=BPPARAM)
		}
		results <- do.call("c", results)
		models <- DataFrame(rev(expand.grid(k=k, r=r)))
		.SpatialKMeans2(
			imageData=.SimpleImageArrayList(),
			featureData=featureData(x),
			elementMetadata=pixelData(x),
			metadata=list(
				resultType=list(
					feature=c("centers", "correlation"),
					pixel="cluster"),
				modelParam=names(models),
				method=method, dist=dist),
			resultData=as(results, "List"),
			modelData=models)
	})

setAs("SpatialKMeans", "SpatialKMeans2",
	function(from) {
		to <- .coerce_ResultImagingExperiment(from, "SpatialKMeans2")
		metadata(to)$resultType <- list(pixel="cluster",
			feature=c("centers", "betweenss", "withinss"))
		to
	})

.spatialKMeans2 <- function(x, r, k, fastmap, seed,
							iter.max, nstart, algorithm, ...)
{
	set.seed(seed)
	progress <- getOption("Cardinal.progress")
	options(Cardinal.progress=FALSE)
	proj <- resultData(fastmap, list(r=r), "scores")
	cluster <- kmeans(proj, centers=k, iter.max=iter.max,
		nstart=nstart, algorithm=algorithm)$cluster
	cluster <- factor(cluster)
	centers <- summarize(x, .stat="mean", .group_by=cluster, ...)$mean
	do_rbind <- function(ans) do.call("rbind", ans)
	corr <- featureApply(x, function(xbl) {
		t(apply(xbl, 1, function(xi) {
			vapply(levels(cluster), function(l) {
				mask <- cluster == l
				if ( all(!mask) ) {
					0
				} else {
					cor(xi, mask)
				}
			}, numeric(1))
		}))
	}, .blocks=TRUE, .simplify=do_rbind, ...)
	options(Cardinal.progress=progress)
	SimpleList(cluster=cluster, centers=centers, correlation=corr)
}
