
#### Spatial shrunken centroids (classification) ####
## -------------------------------------------------

setMethod("spatialShrunkenCentroids", c(x = "ANY", y = "ANY"),
	function(x, y, coord, r = 1, s = 0,
		weights = c("gaussian", "adaptive"),
		neighbors = findNeighbors(coord, r=r), bags = NULL,
		priors = table(y), center = NULL, transpose = TRUE,
		verbose = getCardinalVerbose(), chunkopts = list(),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( "method" %in% ...names() ) {
		.Deprecated(old="method", new="weights")
		weights <- list(...)$method
	}
	y <- as.factor(y)
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
	# calculate global centroid
	if ( is.null(center) ) {
		.Log("calculating global centroid",
			message=verbose)
		if ( transpose ) {
			center <- rowStats(x, stat="mean", na.rm=TRUE,
				verbose=verbose, chunkopts=chunkopts,
				BPPARAM=BPPARAM)
		} else {
			center <- colStats(x, stat="mean", na.rm=TRUE,
				verbose=verbose, chunkopts=chunkopts,
				BPPARAM=BPPARAM)
		}
	}
	if ( transpose ) {
		distfun <- .spatialColDists
	} else {
		distfun <- .spatialRowDists
	}
	if ( is.null(bags) ) {
		ans <- nscentroids(x, y=y, s=s, distfun=distfun,
			priors=priors, center=center, transpose=transpose,
			neighbors=neighbors, neighbors.weights=nbwts,
			verbose=verbose, chunkopts=chunkopts,
			BPPARAM=BPPARAM, ...)
	} else {
		ans <- lapply(s, function(si)
			{
				.Log("fitting values for s = ", si,
					message=verbose)
				mi_learn(nscentroids, x=x, y=y,
					s=si, distfun=distfun, bags=bags, score=fitted,
					priors=priors, center=center, transpose=transpose,
					neighbors=neighbors, neighbors.weights=nbwts,
					verbose=verbose, chunkopts=chunkopts,
					BPPARAM=BPPARAM, ...)
			})
	}
	if ( is(ans, "nscentroids") )
		ans <- list(ans)
	ans <- lapply(ans,
		function(a) {
			a$weights <- weights
			a$r <- r
			a
		})
	names(ans) <- paste0("r=", r, ",s=", s)
	.Log("returning shrunken centroids classification",
		message=verbose)
	if ( length(ans) > 1L ) {
		ResultsList(ans,
			mcols=DataFrame(r=r, s=s, weights=weights))
	} else {
		ans[[1L]]
	}
})

setMethod("spatialShrunkenCentroids", c(x = "SpectralImagingExperiment", y = "ANY"),
	function(x, y, r = 1, s = 0,
		weights = c("gaussian", "adaptive"),
		neighbors = findNeighbors(x, r=r), ...)
{
	if ( length(processingData(x)) > 0L )
		.Warn("queued processing steps will be ignored")
	ans <- spatialShrunkenCentroids(spectra(x), y=y,
		coord=coord(x), r=r, s=s,
		neighbors=neighbors, weights=weights,
		transpose=TRUE, ...)
	if ( is(ans, "ResultsList") ) {
		f <- function(a) as(SpatialResults(a, x), "SpatialShrunkenCentroids")
		ResultsList(lapply(ans, f), mcols=mcols(ans))
	} else {
		as(SpatialResults(ans, x), "SpatialShrunkenCentroids")
	}
})

setMethod("fitted", "SpatialShrunkenCentroids",
	function(object, type = c("response", "class"), ...)
{
	type <- match.arg(type)
	fitted(object@model, type=type)
})

setMethod("predict", "SpatialShrunkenCentroids",
	function(object, newdata,
		type = c("response", "class"),
		weights = object$weights, r = object$r,
		neighbors = findNeighbors(newdata, r=r),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	type <- match.arg(type)
	if ( !missing(newdata) && !is(newdata, "SpectralImagingExperiment") )
		.Error("'newdata' must inherit from 'SpectralImagingExperiment'")
	if ( nrow(newdata) != nrow(object$centers) )
		.Error("'newdata' does not have the correct number of dimensions")
	if ( !missing(newdata) ) {
		if ( length(processingData(newdata)) > 0L )
			.Warn("queued processing steps will be ignored")
		if ( is.character(weights) ) {
			nbwts <- spatialWeights(newdata, r=r,
				neighbors=neighbors, weights=weights,
				BPPARAM=BPPARAM, ...)
		} else {
			nbwts <- rep_len(weights, length(neighbors))
		}
		predict(object@model, newdata=spectra(newdata), type=type,
			neighbors=neighbors, neighbors.weights=nbwts,
			verbose=FALSE, BPPARAM=BPPARAM, ...)
	} else {
		fitted(object@model, type=type)
	}
})

setMethod("logLik", "SpatialShrunkenCentroids",
	function(object, ...) logLik(object@model))

setMethod("topFeatures", "SpatialShrunkenCentroids",
	function(object, n = Inf, sort.by = c("statistic", "centers"), ...)
{
	sort.by <- match.arg(sort.by)
	stats <- object$statistic
	means <- object$centers
	sds <- object$sd
	class <- rep(colnames(stats), each=nrow(stats))
	topf <- DataFrame(class=class, statistic=as.vector(stats),
		centers=as.vector(means), sd=sds)
	topf <- .rank_featureData(object, topf, sort.by)
	head(topf, n=n)
})

setMethod("plot", c(x = "SpatialShrunkenCentroids", y = "missing"),
	function(x, type = c("statistic", "centers"), ..., xlab, ylab)
{
	type <- match.arg(type)
	if ( missing(xlab) )
		xlab <- NULL
	if ( type == "statistic" ) {
		if ( missing(ylab) )
			ylab <- "Statistic"
		callNextMethod(x, y=x$statistic, xlab=xlab, ylab=ylab, ...)
	} else {
		if ( missing(ylab) )
			ylab <- "Shrunken centroids"
		callNextMethod(x, y=x$centers, xlab=xlab, ylab=ylab, ...)
	}
})

setMethod("image", c(x = "SpatialShrunkenCentroids"),
	function(x, type = c("probability", "class"), ...)
{
	type <- match.arg(type)
	if ( type == "probability" ) {
		callNextMethod(x, y=x$probability, ...)
	} else {
		callNextMethod(x, y=x$class, ...)
	}
})


#### Spatial shrunken centroids (clustering) ####
## ---------------------------------------------

setMethod("spatialShrunkenCentroids", c(x = "ANY", y = "missing"),
	function(x, coord, r = 1, k = 2, s = 0,
		weights = c("gaussian", "adaptive"),
		neighbors = findNeighbors(coord, r=r),
		init = NULL, threshold = 0.01, niter = 10L,
		center = NULL, transpose = FALSE,
		verbose = getCardinalVerbose(), chunkopts = list(),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	weights <- match.arg(weights)
	if ( "method" %in% ...names() ) {
		.Deprecated(old="method", new="weights")
		weights <- list(...)$method
	}
	if ( is.character(weights) ) {
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
	# calculate global centroid
	if ( is.null(center) ) {
		.Log("calculating global centroid",
			message=verbose)
		if ( transpose ) {
			center <- rowStats(x, stat="mean", na.rm=TRUE,
				verbose=verbose, chunkopts=chunkopts,
				BPPARAM=BPPARAM)
		} else {
			center <- colStats(x, stat="mean", na.rm=TRUE,
				verbose=verbose, chunkopts=chunkopts,
				BPPARAM=BPPARAM)
		}
	}
	# initialize with k-means
	if ( is.null(init) )
	{
		.Log("initializing clusters with spatial k-means",
			message=verbose)
		init <- spatialKMeans(x, k=k, transpose=transpose,
			neighbors=neighbors, weights=nbwts,
			centers=FALSE, correlation=FALSE,
			verbose=verbose, chunkopts=chunkopts,
			BPPARAM=BPPARAM, ...)
	}
	if ( length(k) == 1L )
		init <- list(init)
	# iterate over parameters
	i <- 1
	s <- sort(s)
	ans <- vector("list", length=length(s) * length(init))
	for ( j in seq_along(init) )
	{
		y <- NULL
		for ( si in s )
		{
			# initialize clusters
			if ( is.null(y) )
			{
				if ( inherits(init[[j]], c("SpatialKMeans", "kmeans")) ) {
					y <- factor(init[[j]]$cluster)
					if ( is.null(init[[j]]$k) ) {
						ki <- nlevels(y)
					} else {
						ki <- init[[j]]$k
					}
				} else {
					y <- as.factor(init[[j]])
					ki <- nlevels(y)
				}
			}
			# perform segmentation
			.Log("fitting k = ", ki, ", s = ", si,
				message=verbose)
			if ( nlevels(y) < ki ) {
				.Log("starting from ", nlevels(y), " clusters",
					message=verbose)
			}
			uprop <- 1
			iter <- 1
			while ( iter <= niter && uprop > threshold )
			{
				fit <- spatialShrunkenCentroids(x, y=y, r=r, s=si,
					priors=1, center=center, transpose=transpose,
					neighbors=neighbors, weights=nbwts,
					verbose=verbose, chunkopts=chunkopts,
					BPPARAM=BPPARAM, ...)
				utot <- sum(fit$class != y)
				uprop <- utot / length(y)
				.Log("clustering iteration ", iter, ": ",
					utot, " cluster assignments updated (",
					round(100 * (1 - uprop), digits=2L), "% complete)",
					message=verbose)
				y <- droplevels(fit$class)
				if ( nlevels(y) < nlevels(fit$class) )
				{
					.Log("clustering iteration ", iter, ": ",
						"number of clusters dropped from ",
						nlevels(fit$class), " to ", nlevels(y),
						message=verbose)
					y <- factor(as.integer(y))
				}
				iter <- iter + 1L
			}
			fit$weights <- weights
			fit$k <- ki
			ans[[i]] <- fit
			i <- i + 1
		}
	}
	# prepare results
	k <- vapply(ans, function(a) a$k, numeric(1L))
	s <- vapply(ans, function(a) a$s, numeric(1L))
	names(ans) <- paste0("r=", r, ",k=", k, ",s=", s)
	.Log("returning shrunken centroids clustering",
		message=verbose)
	if ( length(ans) > 1L ) {
		kout <- vapply(ans, function(a) nlevels(a$class), numeric(1L))
		pz <- vapply(ans, function(a) mean(a$statistic == 0), numeric(1L))
		aic <- vapply(ans, AIC, numeric(1L))
		bic <- vapply(ans, BIC, numeric(1L))
		ResultsList(ans,
			mcols=DataFrame(r=r, k=k, s=s, weights=weights, clusters=kout,
				sparsity=round(pz, digits=2L), AIC=aic, BIC=bic))
	} else {
		ans[[1L]]
	}
})

setMethod("spatialShrunkenCentroids", c(x = "SpectralImagingExperiment", y = "missing"),
	function(x, r = 1, k = 2, s = 0,
		weights = c("gaussian", "adaptive"),
		neighbors = findNeighbors(x, r=r), ...)
{
	if ( length(processingData(x)) > 0L )
		.Warn("queued processing steps will be ignored")
	ans <- spatialShrunkenCentroids(spectra(x),
		coord=coord(x), r=r, k=k, s=s,
		neighbors=neighbors, weights=weights,
		transpose=TRUE, ...)
	f <- function(a) as(SpatialResults(a, x), "SpatialShrunkenCentroids")
	if ( is(ans, "ResultsList") ) {
		ResultsList(lapply(ans, f), mcols=mcols(ans))
	} else {
		f(ans)
	}
})



