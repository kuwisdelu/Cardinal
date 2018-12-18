
# setMethod("spatialShrunkenCentroids", "SparseImagingExperiment",
# 	function(x, r = 1, k = 2,
# 		method = c("gaussian", "adaptive"),
# 		dist = "chebyshev", seed = 1,
# 		BPPARAM = bpparam(), ...)
# 	{
# 		.checkForIncompleteProcessing(x)
# 		BPPARAM <- .protectNestedBPPARAM(BPPARAM)
# 		method <- match.arg(method)
# 		if ( max(k) > ncol(x) )
# 			.stop("can't fit more clusters than number of pixels")
# 		if ( max(k) > nrow(x) )
# 			.stop("can't fit more clusters than number of features")
# 		ncomp <- min(ncomp, nrow(x))
# 		.message("initializing centroids with k-means")
# 		init <- spatialKMeans(x, r=r, k=k,
# 			method=method, dist=dist, BPPARAM=BPPARAM, ...)
# 		results <- list()
# 		par <- expand.grid(k=k, r=r)
# 		.message("fitting spatial shrunken centroids...")
# 		for ( i in nrow(par) ) {
# 			.message("r = ", par$r[i], ", k = ", par$k[i])
# 			results[[i]] <- bplapply(s, function(si, BPPARAM) {
# 				.message("s = ", si, " ", appendLF = FALSE)
# 				.spatialShrunkenCentroids2_cluster(x=x,
# 					r=par$r[i], k=par$k[i], s=si, method=method,
# 					dist=dist, init=init, seed=seed,
# 					BPPARAM=BPPARAM, ...)
# 			}, BPPARAM=BPPARAM)
# 		}
# 		results <- do.call("c", results)
# 		models <- DataFrame(expand.grid(s=s, k=k, r=r))
# 		models <- models[c("r", "k", "s")]
# 		.SpatialKMeans2(
# 			imageData=.SimpleImageArrayList(),
# 			featureData=featureData(x),
# 			elementMetadata=pixelData(x),
# 			metadata=list(resultType=list(
# 				feature=c("centers", "statistic", "sd"),
# 				pixel=c("class", "probability", "scores"))),
# 			resultData=as(results, "List"),
# 			modelData=models)
# 	})


setAs("SpatialShrunkenCentroids", "SpatialShrunkenCentroids2",
	function(from) {
		to <- .coerce_ResultImagingExperiment(from, "SpatialShrunkenCentroids2")
		metadata(to)$resultType <- list(feature=c("centers", "tstatistics"),
			pixel=c("probabilities", "classes", "scores"))
		resultData(to) <- endoapply(resultData(to), function(res) {
			rename(res, tstatistics="statistic", classes="class",
				probabilities="probability")
		})
		to
	})


