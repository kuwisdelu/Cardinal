
# setMethod("spatialKMeans", "SparseImagingExperiment",
# 	function(x, r = 1, k = 2,
# 		method = c("gaussian", "adaptive"),
# 		iter.max = 10, nstart = 1,
# 		algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"),
# 		ncomp = 10, ...)
# 	{
# 		.checkForIncompleteProcessing(x)
# 		BPPARAM <- .protect_nested_BPPARAM(BPPARAM)
# 		method <- match.arg(method)
# 		if ( max(k) > ncol(x) )
# 			.stop("can't fit more clusters than number of pixels")
# 		if ( max(k) > nrow(x) )
# 			.stop("can't fit more clusters than number of features")
# 		ncomp <- min(ncomp, nrow(x))
# 		.message("reducing dimension prior to k-means")
# 		fastmap <- spatialFastmap(x, r=r,
# 			ncomp=ncomp, method=method, iter.max=1)
# 		results <- bplapply(r, function(ri, BPPARAM) {
# 			.message("r = ", ri)
# 			.spatialFastmap2(x=x, r=ri, ncomp=ncomp,
# 				method=method, iter.max=iter.max,
# 				BPPARAM=BPPARAM, ...)
# 		}, BPPARAM=BPPARAM)
# 		models <- DataFrame(r=r, ncomp=ncomp)
# 		.SpatialKMeans2(
# 			imageData=.SimpleImageArrayList(),
# 			featureData=featureData(x),
# 			elementMetadata=pixelData(x),
# 			metadata=list(resultType=list(
# 				feature="centers", "correlation",
# 				pixel="cluster")),
# 			resultData=as(results, "List"),
# 			modelData=models)
# 	})

setAs("SpatialKMeans", "SpatialKMeans2",
	function(from) {
		to <- .coerce_ResultImagingExperiment(from, "SpatialKMeans2")
		metadata(to)$resultType <- list(pixel="cluster",
			feature=c("centers", "betweenss", "withinss"))
		to
	})


