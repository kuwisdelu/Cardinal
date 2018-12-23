
# setMethod("spatialShrunkenCentroids", "SparseImagingExperiment",
# 	function(x, r = 1, k = 2,
# 		method = c("gaussian", "adaptive"),
# 		dist = "chebyshev", droplevels = TRUE,
# 		seed = 1, iter.max = 10,
# 		BPPARAM = bpparam(), ...)
# 	{
# 		.checkForIncompleteProcessing(x)
# 		BPPARAM <- .protectNestedBPPARAM(BPPARAM)
# 		method <- match.arg(method)
# 		if ( max(k) > ncol(x) )
# 			.stop("can't fit more clusters than number of pixels")
# 		if ( max(k) > nrow(x) )
# 			.stop("can't fit more clusters than number of features")
# 		.message("initializing centroids with k-means")
# 		init <- spatialKMeans(x, r=r, k=k, method=method,
# 			dist=dist, seed=seed, BPPARAM=BPPARAM, ...)
# 		mean <- summarize(x, .stat="mean", BPPARAM=BPPARAM)$mean
# 		results <- list()
# 		par <- expand.grid(k=k, r=r)
# 		.message("fitting spatial shrunken centroids...")
# 		for ( i in nrow(par) ) {
# 			.message("r = ", par$r[i], ", k = ", par$k[i])
# 			.message("calculating spatial weights...")
# 			spatial <- .spatialInfo(x, r=r, method=method, dist=dist)
# 			weights <- spatial$weights
# 			results[[i]] <- bplapply(s, function(si, BPPARAM) {
# 				.message("s = ", si, " ", appendLF = FALSE)
# 				init.class <- resultData(init,
# 					list(r=par$r[i], k=par$k[i]), "cluster")
# 				.spatialShrunkenCentroids2_cluster(x=x,
# 					r=par$r[i], k=par$k[i], s=si, mean=mean,
# 					class=init.class, weights=weights, dist=dist,
# 					droplevels=droplevels, iter.max=iter.max,
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

# .spatialShrunkenCentroids2_cluster <- function(x, r, k, s, mean, class, weights,
# 									dist, droplevels, iter.max, BPPARAM, ...)
# {
# 	iter <- 1
# 	init <- TRUE
# 	verbose <- getOption("Cardinal.verbose")
# 	progress <- getOption("Cardinal.progress")
# 	options(Cardinal.verbose=FALSE)
# 	options(Cardinal.progress=FALSE)
# 	while ( iter <= iter.max ) {
# 		if ( droplevels )
# 			class <- droplevels(class)
# 		priors <- table(class) / sum(table(class))
# 		fit <- .spatialShrunkenCentroids2_fit(x, s=s,
# 			mean=mean, class=class, init=init,
# 			BPPARAM=BPPARAM)
# 		pred <- .spatialShrunkenCentroids2_predict(x,
# 			mean=mean, class=class, weights=weights,
# 			centers=fit$centers, sd=fit$sd,
# 			priors=priors, init=fit$init,
# 			BPPARAM=BPPARAM)
# 		class <- pred$class
# 		init <- pred$init
# 		iter <- iter + 1
# 	}
# 	options(Cardinal.verbose=verbose)
# 	options(Cardinal.progress=progress)
# 	list(class=pred$class, probability=pred$probability, scores=pred$scores,
# 		centers=fit$centers, statistic=fit$statistic, sd=fit$sd)
# }

# .spatialShrunkenCentroids2_fit <- function(x, s, mean, class, init, BPPARAM, ...)
# {
# 	# calculate class centers
# 	centers <- summarize(x, .stat="mean", .group.by=class, BPPARAM=BPPARAM)
# 	centers <- as.matrix(centers, slots=FALSE)
# 	colnames(centers) <- levels(class)
# 	# calculate within-class pooled SE
# 	# 	do something
# }

# .spatialShrunkenCentroids2_predict <- function(x, mean, class, weights,
# 									centers, sd, priors, init, BPPARAM, ...)
# {
	
# }

