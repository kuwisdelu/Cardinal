
setMethod("spatialKMeans", "SparseImagingExperiment",
	function(x, r = 1, k = 3,
		method = c("gaussian", "adaptive"),
		dist = "chebyshev", tol.dist = 1e-9,
		iter.max = 10, nstart = 10,
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
		fastmap <- spatialFastmap(x, r=r, ncomp=ncomp,
			method=method, dist=dist, tol.dist=tol.dist,
			iter.max=1, BPPARAM=BPPARAM, ...)
		metric <- metadata(fastmap)$metric
		results <- list()
		.message("clustering components...")
		for ( i in seq_along(r) ) {
			rngseeds <- generateRNGStreams(length(k))
			results[[i]] <- bpmapply(function(ki, seed, BPPARAM) {
				.message("r = ", r[i], ", k = ", ki)
				.spatialKMeans2(x=x, r=r[i], k=ki, fastmap=fastmap,
					seed=seed, iter.max=iter.max, nstart=nstart,
					algorithm=algorithm, BPPARAM=BPPARAM)
			}, k, rngseeds, SIMPLIFY=FALSE, BPPARAM=BPPARAM)
		}
		results <- do.call("c", results)
		models <- DataFrame(rev(expand.grid(k=k, r=r)))
		.SpatialKMeans2(
			imageData=.SimpleImageArrayList(),
			featureData=featureData(x),
			elementMetadata=pixelData(x),
			metadata=list(
				mapping=list(
					feature=c("centers", "correlation"),
					pixel="cluster"),
				method=method, dist=dist,
				metric=metric),
			resultData=as(results, "List"),
			modelData=models)
	})

setAs("SpatialKMeans", "SpatialKMeans2",
	function(from) {
		to <- .coerce_ImagingResult(from, "SpatialKMeans2")
		metadata(to)$mapping <- list(pixel="cluster",
			feature=c("centers", "betweenss", "withinss"))
		metadata(to)$method <- resultData(to, 1, "method")
		metadata(to)$distance <- "chebyshev"
		to
	})

.spatialKMeans2 <- function(x, r, k, fastmap, seed, iter.max,
									nstart, algorithm, BPPARAM)
{
	oseed <- getRNGStream()
	on.exit(setRNGStream(oseed))
	setRNGStream(seed)
	# cluster FastMap components using k-means
	proj <- resultData(fastmap, list(r=r), "scores")
	cluster <- kmeans(proj, centers=k, iter.max=iter.max,
		nstart=nstart, algorithm=algorithm)$cluster
	cluster <- factor(cluster)
	centers <- rowStats(iData(x), stat="mean", groups=cluster,
		chunks=getOption("Cardinal.numblocks"),
		verbose=FALSE, BPPARAM=BPPARAM)
	colnames(centers) <- levels(cluster)
	# calculate correlation with clusters
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
	}, .simplify=do_rbind, .verbose=FALSE, view="chunk", BPPARAM=BPPARAM)
	colnames(corr) <- levels(cluster)
	list(cluster=cluster, centers=centers, correlation=corr)
}
