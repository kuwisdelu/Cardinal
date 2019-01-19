
setMethod("spatialShrunkenCentroids",
	signature = c("SparseImagingExperiment", "missing"),
	function(x, r = 1, k = 2, s = 0,
		method = c("gaussian", "adaptive"),
		dist = "chebyshev", droplevels = TRUE,
		init = NULL, iter.init = 10,
		seed = 1, iter.max = 10,
		BPPARAM = bpparam(), ...)
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
			init <- suppressWarnings(spatialKMeans(x, r=r, k=k, method=method,
				dist=dist, seed=seed, iter.max=iter.init, BPPARAM=BPPARAM, ...))
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
		for ( i in nrow(par) ) {
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
					class=init.class, weights=weights, dist=dist,
					droplevels=droplevels, iter.max=iter.max,
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
				resultType=list(
					feature=c("centers", "statistic", "sd"),
					pixel=c("scores", "probability", "class")),
				modelParam=c("r", "k", "s"),
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
		e <- as.env(featureData(x), enclos=parent.frame(2))
		y <- .try_eval(substitute(y), envir=e)
		if ( is.character(y) && length(y) == 1L && y %in% names(fData(x)) ) {
			yname <- y
			y <- as.factor(featureData(x)[[yname]])
		} else {
			yname <- "response"
			y <- as.factor(y)
		}
		.message("calculating global centroid...")
		mean <- summarize(x, .stat="mean", BPPARAM=BPPARAM[[1]])$mean
		par <- expand.grid(s=s, r=r)
		.message("fitting spatial shrunken centroids...")
		results <- bpmapply(function(ri, si) {
			.message("r = ", ri, ", ", "s = ", si, " ", appendLF = FALSE)
			weights <- r.weights$w[[which(r.weights$r == par$ri)]]
			.spatialShrunkenCentroids2_fit(x, s=s,
				mean=mean, class=y, BPPARAM=BPPARAM)
		}, SIMPLIFY=FALSE, BPPARAM=BPPARAM)
		models <- DataFrame(rev(expand.grid(s=s, r=r)))
		out <- .SpatialShrunkenCentroids2(
			imageData=.SimpleImageArrayList(),
			featureData=featureData(x),
			elementMetadata=pixelData(x),
			metadata=list(
				resultType=list(
					feature=c("centers", "statistic", "sd"),
					pixel=c("scores", "probability", "class")),
				modelParam=c("r", "s"),
				responseName=yname,
				method=method, dist=dist,
				priors=priors / sum(priors)),
			resultData=as(results, "List"),
			modelData=models)
		modelData(out)$num_features <- sapply(results, function(res) {
			round(mean(colSums(res$statistic != 0)), 1)
		})
		featureData(out)[[yname]] <- y
		predict(out, newx=x, method=method, BPPARAM=BPPARAM)
	})

setMethod("predict", "SpatialShrunkenCentroids2",
	function(object, newx, newy, r = 1, method = c("gaussian", "adaptive"),
		dist = "chebyshev", priors = NULL, BPPARAM = bpparam(), ...)
	{
		if ( !is(newx, "SparseImagingExperiment") )
			.stop("'newx' must inherit from 'SparseImagingExperiment'")
		.checkForIncompleteProcessing(x)
		BPPARAM <- .protectNestedBPPARAM(BPPARAM)
		e <- as.env(featureData(x), enclos=parent.frame(2))
		if ( missing(newy) ) {
			if ( !is.null(metadata(object)$responseName) ) {
				newy <- featureData(object)[[metadata(object)$responseName]]
			} else {
				newy <- NULL
			}
		} else {
			newy <- .try_eval(substitute(newy), envir=e)
			if ( is.character(newy) && length(newy) == 1L && newy %in% names(fData(object)) ) {
				yname <- newy
				newy <- as.factor(featureData(object)[[yname]])
			} else {
				yname <- "response"
				newy <- as.factor(newy)
			}
		}
		if ( missing(r) )
			r <- modelData(object)$r
		if ( missing(method) && !is.null(metadata(object)$method) ) {
			method <- metadata(object)$method
		} else {
			method <- match.arg(method)
		}
		if ( missing(dist) && !is.null(metadata(object)$dist) )
			dist <- metadata(object)$dist
		if ( missing(priors) && !is.null(metadata(object)$priors) )
			priors <- metadata(object)$priors
		.message("calculating global centroid...")
		mean <- summarize(newx, .stat="mean", BPPARAM=BPPARAM[[1]])$mean
		.message("calculating spatial weights...")
		r.weights <- list(r=r, w=lapply(r, function(ri) {
			spatialWeights(newx, r=ri, method=method,
				dist=dist, BPPARAM=BPPARAM[[1]])
		}))
		results <- bplapply(resultData(object), function(res) {
			weights <- r.weights$w[[which(r.weights$r == par$r[i])]]
			if ( is.null(newy) ) {
				class <- factor(seq_len(ncol(res$centers)))
			} else {
				class <- newy
			}
			pred <- .spatialShrunkenCentroids2_predict(x,
				r=r, mean=mean, class=class, weights=weights,
				centers=fit$centers, sd=fit$sd, priors=priors,
				init=NULL, BPPARAM=BPPARAM)
			list(class=pred$class, probability=pred$probability, scores=pred$scores,
				centers=res$centers, statistic=res$statistic, sd=res$sd)
		}, BPPARAM=BPPARAM)
		if ( !is.null(newy) ) {
			modelData(object)$accuracy <- sapply(results, function(res) {
				mean(res$class == newy, na.rm=TRUE)
			})
		}
		resultData(object) <- as(results, "List")
		object
	})

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

.spatialShrunkenCentroids2_cluster <- function(x, r, k, s, mean, class, weights,
									dist, droplevels, iter.max, BPPARAM, ...)
{
	iter <- 1
	init <- TRUE
	progress <- getOption("Cardinal.progress")
	options(Cardinal.progress=FALSE)
	while ( iter <= iter.max ) {
		if ( droplevels )
			class <- factor(as.integer(droplevels(class)))
		fit <- .spatialShrunkenCentroids2_fit(x,
			s=s, mean=mean, class=class, BPPARAM=BPPARAM)
		pred <- .spatialShrunkenCentroids2_predict(x,
			r=r, mean=mean, class=class, weights=weights,
			centers=fit$centers, sd=fit$sd, priors=NULL,
			init=init, BPPARAM=BPPARAM)
		class <- pred$class
		init <- pred$init
		iter <- iter + 1
		.message(".", appendLF=FALSE)
	}
	.message(" ")
	options(Cardinal.progress=progress)
	list(class=pred$class, probability=pred$probability, scores=pred$scores,
		centers=fit$centers, statistic=fit$statistic, sd=fit$sd)
}

.spatialShrunkenCentroids2_fit <- function(x, s, mean, class, BPPARAM, ...)
{
	verbose <- getOption("Cardinal.verbose")
	options(Cardinal.verbose=FALSE)
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
	options(Cardinal.verbose=verbose)
	list(centers=centers, statistic=statistic, sd=sd)
}

.spatialShrunkenCentroids2_predict <- function(x, r, mean, class, weights,
									centers, sd, priors, init, BPPARAM, ...)
{
	# calculate spatial discriminant scores
	fun <- function(xbl) {
		idx <- attr(xbl, "idx")
		f <- function(i, neighbors, weights) {
			.Call("spatialZScores", xbl[,neighbors,drop=FALSE],
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
	if ( is.null(priors) )
		priors <- table(class) / sum(table(class))
	scores <- t(scores) - 2 * log(rep(priors, each=ncol(x)))
	colnames(scores) <- levels(class)
	# calculate posterior class probabilities
	probability <- t(apply(scores, 1, function(sc) {
		sc <- exp(-0.5 * sc) / sum(exp(-0.5 * sc))
		pmax(sc, 0, na.rm=TRUE)
	}))
	colnames(probability) <- levels(class)
	# calculate and return new class assignments
	class <- factor(apply(probability, 1, which.max),
		levels=seq_len(nlevels(class)), labels=levels(class))
	list(scores=scores, probability=probability, class=class, init=init)
}

