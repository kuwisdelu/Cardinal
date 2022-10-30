
setMethod("spatialShrunkenCentroids",
	signature = c("SparseImagingExperiment", "missing"),
	function(x, r = 1, k = 3, s = 0,
		method = c("gaussian", "adaptive"),
		dist = "chebyshev", init = NULL,
		iter.max = 10, BPPARAM = getCardinalBPPARAM(), ...)
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
		mean <- rowStats(iData(x), stat="mean",
			nchunks=getCardinalNumBlocks(),
			verbose=FALSE, BPPARAM=BPPARAM[[1]])
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
					dist=dist, iter.max=iter.max,
					BPPARAM=BPPARAM, ...)
			}, BPPARAM=BPPARAM)
		}
		results <- do.call("c", results)
		models <- DataFrame(rev(expand.grid(s=s, k=k, r=r)))
		.SpatialShrunkenCentroids2(
			imageData=.SimpleImageArrayList(),
			featureData=featureData(x),
			elementMetadata=pixelData(x),
			metadata=list(
				mapping=list(
					feature=c("centers", "statistic", "sd"),
					pixel=c("scores", "probability", "class")),
				method=method, dist=dist),
			resultData=as(results, "List"),
			modelData=models)
	})


setMethod("spatialShrunkenCentroids",
	signature = c("SparseImagingExperiment", "ANY"),
	function(x, y, r = 1, s = 0,
		method = c("gaussian", "adaptive"),
		dist = "chebyshev", priors = table(y),
		BPPARAM = getCardinalBPPARAM(), ...)
	{
		.checkForIncompleteProcessing(x)
		BPPARAM <- .protectNestedBPPARAM(BPPARAM)
		method <- match.arg(method)
		y <- as.factor(y)
		.message("calculating global centroid...")
		mean <- rowStats(iData(x), stat="mean",
			nchunks=getCardinalNumBlocks(),
			verbose=FALSE, BPPARAM=BPPARAM[[1]])
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
				method=method, dist=dist,
				priors=priors / sum(priors)),
			resultData=as(results, "List"),
			modelData=models)
		predict(out, newx=x, newy=y, method=method, BPPARAM=BPPARAM)
	})

setMethod("predict", "SpatialShrunkenCentroids2",
	function(object, newx, newy, BPPARAM = getCardinalBPPARAM(), ...)
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
				sd=res$sd, priors=priors, dist=dist, BPPARAM=BPPARAM)
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
		if ( !missing(newy) )
			pixelData(out)$.response <- newy
		out
	})

setMethod("fitted", "SpatialShrunkenCentroids2",
	function(object, ...) object$class)

setMethod("logLik", "SpatialShrunkenCentroids2", function(object, ...) {
	logp <- sapply(object$probability, function(prob) {
		phat <- apply(prob, 1, max)
		sum(log(phat), na.rm=TRUE)
	})
	class(logp) <- "logLik"
	attr(logp, "df") <- sapply(object$statistic, function(t) {
		sum(abs(t) > 0) + length(features(object))
	})
	attr(logp, "nobs") <- length(pixels(object))
	logp
})

setAs("SpatialShrunkenCentroids", "SpatialShrunkenCentroids2",
	function(from) {
		to <- .coerce_ImagingResult(from, "SpatialShrunkenCentroids2")
		metadata(to)$mapping <- list(feature=c("centers", "tstatistics"),
			pixel=c("probabilities", "classes", "scores"))
		resultData(to) <- endoapply(resultData(to), function(res) {
			res <- rename(res, tstatistics="statistic", classes="class",
				probabilities="probability")
			if ( is.null(res$y) ) {
				res$class <- droplevels(res$class)
				i <- seq_along(levels(res$class))
				res$centers <- res$centers[,i,drop=FALSE]
				res$statistic <- res$statistic[,i,drop=FALSE]
				res$probability <- res$probability[,i,drop=FALSE]
				res$scores <- res$scores[,i,drop=FALSE]
			}
			res
		})
		metadata(to)$method <- resultData(to, 1, "method")
		metadata(to)$distance <- "chebyshev"
		if ( !is.null(resultData(to, 1, "y")) )
			pixelData(to)$.response <- resultData(to, 1, "y")
		to
	})

.spatialShrunkenCentroids2_cluster <- function(x, r, k, s, mean, class, weights,
												dist, iter.max, BPPARAM)
{
	iter <- 1
	# cluster the data
	while ( iter <= iter.max ) {
		class <- factor(as.integer(droplevels(class)))
		fit <- .spatialShrunkenCentroids2_fit(x,
			s=s, mean=mean, class=class, BPPARAM=BPPARAM)
		pred <- .spatialShrunkenCentroids2_predict(x,
			r=r, class=class, weights=weights, centers=fit$centers,
			sd=fit$sd, priors=NULL, dist=dist, BPPARAM=BPPARAM)
		converged <- all(class==pred$class)
		singular <- nlevels(droplevels(pred$class)) == 1L
		failed <- anyNA(pred$class)
		if ( failed )
			.warning("failed to converge: some predictions will be missing")
		if ( singular )
			.warning("failed to converge: singular cluster configuration")
		if ( failed || singular || converged )
			break
		class <- pred$class
		iter <- iter + 1
		.message(".", appendLF=FALSE)
	}
	.message(" ")
	list(class=pred$class, probability=pred$probability, scores=pred$scores,
		centers=fit$centers, statistic=fit$statistic, sd=fit$sd)
}

.spatialShrunkenCentroids2_fit <- function(x, s, mean, class, BPPARAM)
{
	# calculate class centers
	centers <- rowStats(iData(x), stat="mean", group=class,
		nchunks=getCardinalNumBlocks(),
		verbose=FALSE, BPPARAM=BPPARAM)
	colnames(centers) <- levels(class)
	# calculate within-class pooled SE
	centered <- rowsweep(iData(x)^2, STATS=centers, group=class)
	wcss <- rowStats(centered, stat="sum", group=class,
					nchunks=getCardinalNumBlocks(),
					verbose=FALSE, BPPARAM=BPPARAM)
	# calculate standard errors
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
							centers, sd, priors, dist, BPPARAM)
{
	# calculate spatial discriminant scores
	fun <- function(xbl) {
		ii <- which(!vapply(attr(xbl, "depends"), is.null, logical(1)))
		di <- attr(xbl, "index")[ii]
		.spatialScores(xbl, centers=centers, weights=weights[di],
			neighbors=attr(xbl, "depends")[ii], sd=sd + s0)
	}
	s0 <- median(sd)
	neighbors <- findNeighbors(x, r=r, dist=dist)
	scores <- chunk_colapply(iData(x), FUN=fun, depends=neighbors,
		simplify=rbind, verbose=FALSE,
		nchunks=getCardinalNumBlocks(),
		BPPARAM=BPPARAM)
	if ( is.null(priors) && !is.null(class) )
		priors <- table(class) / sum(table(class))
	scores <- scores - 2 * log(rep(priors, each=ncol(x)))
	colnames(scores) <- colnames(centers)
	# calculate posterior class probabilities
	pfun <- function(scores) {
		s <- exp(-0.5 * scores)
		pmax(s / rowSums(s), 0)
	}
	probability <- pfun(scores)
	colnames(probability) <- colnames(centers)
	# calculate and return new class assignments
	pred <- function(s) {
		i <- which.min(s)
		if ( length(i) == 0L ) {
			NA_integer_
		} else {
			i
		}
	}
	class <- factor(apply(scores, 1, pred),
		levels=seq_len(ncol(centers)),
		labels=colnames(centers))
	list(scores=scores, probability=probability, class=class)
}

