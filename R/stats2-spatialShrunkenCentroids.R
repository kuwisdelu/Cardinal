
setMethod("spatialShrunkenCentroids",
	signature = c("SparseImagingExperiment", "missing"),
	function(x, r = 1, k = 3, s = 0,
		method = c("gaussian", "adaptive"),
		dist = "chebyshev", init = NULL,
		iter.max = 10, BPPARAM = bpparam(), ...)
	{
		.checkForIncompleteProcessing(x)
		BPPARAM <- .protectNestedBPPARAM(BPPARAM)
		method <- match.arg(method)
		if ( max(k) > ncol(x) )
			.stop("can't fit more clusters than number of pixels")
		if ( max(k) > nrow(x) )
			.stop("can't fit more clusters than number of features")
		if ( !is.null(init) ) {
			if ( !is(init, "SpatialKMeans2") ) {
				if ( !is.list(init) )
					init <- list(init)
				init <- lapply(init, as.factor)
				k <- sapply(init, nlevels)
				init <- list(k=k, class=init)
			}
		} else {
			.message("initializing shrunken centroids clustering with k-means")
			init <- suppressWarnings(spatialKMeans(x, r=r, k=k,
				method=method, dist=dist, BPPARAM=BPPARAM, ...))
		}
		.message("calculating global centroid...")
		mean <- summarize(x, .stat="mean", BPPARAM=BPPARAM[[1]])$mean
		.message("calculating spatial weights...")
		r.weights <- list(r=r, w=lapply(r, function(ri) {
			spatialWeights(x, r=ri, method=method,
				dist=dist, BPPARAM=BPPARAM[[1]])
		}))
		results <- list()
		par <- expand.grid(k=k, r=r)
		.message("fitting spatial shrunken centroids...")
		for ( i in 1:nrow(par) ) {
			weights <- r.weights$w[[which(r.weights$r == par$r[i])]]
			if ( is.list(init) ) {
				init.class <- init$class[[which(init$k == par$k[i])]]
			} else {
				init.class <- resultData(init,
					list(r=par$r[i], k=par$k[i]), "cluster")
			}
			results[[i]] <- bplapply(s, function(si, BPPARAM) {
				.message("r = ", par$r[i], ", k = ", par$k[i],
					", ", "s = ", si, " ", appendLF = FALSE)
				.spatialShrunkenCentroids2_cluster(x=x,
					r=par$r[i], k=par$k[i], s=si, mean=mean,
					class=init.class, weights=weights,
					drop.empty=TRUE, iter.max=iter.max,
					BPPARAM=BPPARAM, ...)
			}, BPPARAM=BPPARAM)
		}
		results <- do.call("c", results)
		models <- DataFrame(rev(expand.grid(s=s, k=k, r=r)))
		models$num_segments <- sapply(results, function(res) {
			nlevels(res$class)
		})
		models$num_features <- sapply(results, function(res) {
			round(mean(colSums(res$statistic != 0)), 1)
		})
		.SpatialShrunkenCentroids2(
			imageData=.SimpleImageArrayList(),
			featureData=featureData(x),
			elementMetadata=pixelData(x),
			metadata=list(
				mapping=list(
					feature=c("centers", "statistic", "sd"),
					pixel=c("scores", "probability", "class")),
				parameters=c("r", "k", "s"),
				method=method),
			resultData=as(results, "List"),
			modelData=models)
	})


setMethod("spatialShrunkenCentroids",
	signature = c("SparseImagingExperiment", "ANY"),
	function(x, y, r = 1, s = 0,
		method = c("gaussian", "adaptive"),
		dist = "chebyshev", priors = table(y),
		BPPARAM = bpparam(), ...)
	{
		.checkForIncompleteProcessing(x)
		BPPARAM <- .protectNestedBPPARAM(BPPARAM)
		method <- match.arg(method)
		y <- as.factor(y)
		.message("calculating global centroid...")
		mean <- summarize(x, .stat="mean", BPPARAM=BPPARAM[[1]])$mean
		.message("calculating spatial weights...")
		r.weights <- list(r=r, w=lapply(r, function(ri) {
			spatialWeights(x, r=ri, method=method,
				dist=dist, BPPARAM=BPPARAM[[1]])
		}))
		.message("fitting spatial shrunken centroids...")
		par <- expand.grid(s=s, r=r)
		results <- bpmapply(function(ri, si, BPPARAM) {
			.message("r = ", ri, ", ", "s = ", si, " ")
			weights <- r.weights$w[[which(r.weights$r == ri)]]
			.spatialShrunkenCentroids2_fit(x, s=si,
				mean=mean, class=y, BPPARAM=BPPARAM)
		}, par$r, par$s, SIMPLIFY=FALSE, BPPARAM=BPPARAM)
		models <- DataFrame(rev(expand.grid(s=s, r=r)))
		out <- .SpatialShrunkenCentroids2(
			imageData=.SimpleImageArrayList(),
			featureData=featureData(x),
			elementMetadata=pixelData(x),
			metadata=list(
				mapping=list(
					feature=c("centers", "statistic", "sd"),
					pixel=c("scores", "probability", "class")),
				parameters=c("r", "s"),
				method=method, dist=dist,
				priors=priors / sum(priors)),
			resultData=as(results, "List"),
			modelData=models)
		modelData(out)$num_features <- sapply(results, function(res) {
			round(mean(colSums(res$statistic != 0)), 1)
		})
		predict(out, newx=x, method=method, BPPARAM=BPPARAM)
	})

setMethod("predict", "SpatialShrunkenCentroids2",
	function(object, newx, newy, BPPARAM = bpparam(), ...)
	{
		if ( !is(newx, "SparseImagingExperiment") )
			.stop("'newx' must inherit from 'SparseImagingExperiment'")
		.checkForIncompleteProcessing(newx)
		BPPARAM <- .protectNestedBPPARAM(BPPARAM)
		if ( !missing(newy) )
			newy <- as.factor(newy)
		r <- modelData(object)$r
		s <- modelData(object)$s
		method <- metadata(object)$method
		dist <- metadata(object)$dist
		priors <- metadata(object)$priors
		.message("calculating spatial weights...")
		r.weights <- list(r=unique(r), w=lapply(r, function(ri) {
			spatialWeights(newx, r=ri, method=method,
				dist=dist, BPPARAM=BPPARAM[[1]])
		}))
		.message("predicting using spatial shrunken centroids...")
		results <- bpmapply(function(res, ri, si, BPPARAM) {
			.message("r = ", ri, ", ", "s = ", si, " ")
			weights <- r.weights$w[[which(r.weights$r == ri)]]
			pred <- .spatialShrunkenCentroids2_predict(newx,
				r=ri, class=NULL, weights=weights, centers=res$centers,
				sd=res$sd, priors=priors, init=NULL, BPPARAM=BPPARAM)
			list(class=pred$class, probability=pred$probability, scores=pred$scores,
				centers=res$centers, statistic=res$statistic, sd=res$sd)
		}, resultData(object), r, s, SIMPLIFY=FALSE, BPPARAM=BPPARAM)
		out <- .SpatialShrunkenCentroids2(
			imageData=.SimpleImageArrayList(),
			featureData=featureData(newx),
			elementMetadata=pixelData(newx),
			metadata=metadata(object),
			resultData=as(results, "List"),
			modelData=modelData(object))
		modelData(out)$num_features <- sapply(results, function(res) {
			round(mean(colSums(res$statistic != 0)), 1)
		})
		if ( !missing(newy) ) {
			modelData(out)$accuracy <- sapply(results,
				function(res) mean(res$class == newy, na.rm=TRUE))
			pixelData(out)$..response.. <- newy
		}
		out
	})

setMethod("fitted", "SpatialShrunkenCentroids2",
	function(object, ...) object$class)

setAs("SpatialShrunkenCentroids", "SpatialShrunkenCentroids2",
	function(from) {
		to <- .coerce_ResultImagingExperiment(from, "SpatialShrunkenCentroids2")
		metadata(to)$mapping <- list(feature=c("centers", "tstatistics"),
			pixel=c("probabilities", "classes", "scores"))
		metadata(to)$parameters <- c("r", "k", "s")
		resultData(to) <- endoapply(resultData(to), function(res) {
			rename(res, tstatistics="statistic", classes="class",
				probabilities="probability")
		})
		modelData(to)$num_features <- sapply(resultData(to), function(res) {
			round(mean(colSums(res$statistic != 0)), 1)
		})
		if ( !is.null(resultData(to, 1, "y")) )
			pixelData(to)$..response.. <- resultData(to, 1, "y")
		to
	})

.spatialShrunkenCentroids2_cluster <- function(x, r, k, s, mean, class, weights,
											drop.empty, iter.max, BPPARAM, ...)
{
	iter <- 1
	init <- TRUE
	# suppress progress in inner parallel loop
	progress <- getOption("Cardinal.progress")
	options(Cardinal.progress=FALSE)
	on.exit(options(Cardinal.progress=progress))
	# suppress messages
	verbose <- getOption("Cardinal.verbose")
	# cluster the data
	while ( iter <= iter.max ) {
		options(Cardinal.verbose=FALSE) # suppress messages
		if ( drop.empty )
			class <- factor(as.integer(droplevels(class)))
		if ( nlevels(class) == 1L )
			.stop("singular cluster configuration, try a different seed")
		fit <- .spatialShrunkenCentroids2_fit(x,
			s=s, mean=mean, class=class, BPPARAM=BPPARAM)
		pred <- .spatialShrunkenCentroids2_predict(x,
			r=r, class=class, weights=weights, centers=fit$centers,
			sd=fit$sd, priors=NULL, init=init, BPPARAM=BPPARAM)
		if ( all(class==pred$class) )
			break
		class <- pred$class
		init <- pred$init
		iter <- iter + 1
		options(Cardinal.verbose=verbose) # print progress now
		.message(".", appendLF=FALSE)
	}
	options(Cardinal.verbose=verbose) # restore
	.message(" ")
	list(class=pred$class, probability=pred$probability, scores=pred$scores,
		centers=fit$centers, statistic=fit$statistic, sd=fit$sd)
}

.spatialShrunkenCentroids2_fit <- function(x, s, mean, class, BPPARAM, ...)
{
	# suppress progress in inner parallel loop
	progress <- getOption("Cardinal.progress")
	options(Cardinal.progress=FALSE)
	on.exit(options(Cardinal.progress=progress))
	# suppress progress in inner parallel loop
	verbose <- getOption("Cardinal.verbose")
	options(Cardinal.verbose=FALSE)
	on.exit(options(Cardinal.verbose=verbose))
	# calculate class centers
	centers <- summarize(x, .stat="mean", .group_by=class, BPPARAM=BPPARAM)
	centers <- as.matrix(centers, slots=FALSE)
	colnames(centers) <- levels(class)
	# calculate within-class pooled SE
	iclass <- as.integer(class)
	tform <- function(xbl) {
		i <- attr(xbl, "idx")
		switch(attr(xbl, "by"),
			"feature" = (xbl - centers[i,iclass,drop=FALSE])^2,
			"pixel" = (xbl - centers[,iclass[i],drop=FALSE])^2)
	}
	# calculate standard errors
	wcss <- summarize(x, .stat="sum", .group_by=class, .tform=tform, BPPARAM=BPPARAM)
	wcss <- as.matrix(wcss, slots=FALSE)
	sd <- sqrt(rowSums(wcss, na.rm=TRUE) / (length(class) - nlevels(class)))
	s0 <- median(sd)
	m.k <- sqrt((1 / table(class)) - (1 / length(class)))
	se <- (sd + s0) * rep(m.k, each=nrow(x))
	dim(se) <- c(nrow(x), nlevels(class))
	# calculate shrunken t-statistics
	statistic <- (centers - mean) / se
	statistic <- soft(statistic, s)
	colnames(statistic) <- levels(class)
	# calculate and return shrunken centroids
	centers <- mean + se * statistic
	colnames(centers) <- levels(class)
	list(centers=centers, statistic=statistic, sd=sd)
}

.spatialShrunkenCentroids2_predict <- function(x, r, class, weights,
							centers, sd, priors, init, BPPARAM, ...)
{
	# suppress progress in inner parallel loop
	progress <- getOption("Cardinal.progress")
	options(Cardinal.progress=FALSE)
	on.exit(options(Cardinal.progress=progress))
	# calculate spatial discriminant scores
	fun <- function(xbl) {
		idx <- attr(xbl, "idx")
		f <- function(i, neighbors, weights) {
			.Call("C_spatialZScores", xbl[,neighbors,drop=FALSE],
				centers, weights, sd + s0, PACKAGE="Cardinal")
		}
		mapply(f, attr(xbl, "centers"),
			attr(xbl, "neighbors"), attr(xbl, "params"))
	}
	s0 <- median(sd)
	scores <- spatialApply(x, .r=r, .fun=fun, .blocks=TRUE,
		.simplify=.cbind_and_reorder, .init=init,
		.params=weights, BPPARAM=BPPARAM)
	if ( isTRUE(init) ) {
		init <- attr(scores, "init")
		attr(scores, "init") <- NULL
	}
	if ( is.null(priors) && !is.null(class) )
		priors <- table(class) / sum(table(class))
	scores <- t(scores) - 2 * log(rep(priors, each=ncol(x)))
	colnames(scores) <- colnames(centers)
	# calculate posterior class probabilities
	probability <- t(apply(scores, 1, function(sc) {
		sc <- exp(-0.5 * sc) / sum(exp(-0.5 * sc))
		pmax(sc, 0, na.rm=TRUE)
	}))
	colnames(probability) <- colnames(centers)
	# calculate and return new class assignments
	class <- factor(apply(probability, 1, which.max),
		levels=seq_len(ncol(centers)), labels=colnames(centers))
	list(scores=scores, probability=probability, class=class, init=init)
}

