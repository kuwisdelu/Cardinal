
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
		if ( verbose )
			message("calculating gaussian weights")
		wts <- spatialWeights(as.matrix(coord), neighbors=neighbors)
		if ( weights == "adaptive" )
		{
			if ( verbose )
				message("calculating adaptive weights")
			awts <- spatialWeights(x, neighbors=neighbors,
				weights="adaptive", byrow=!transpose,
				verbose=verbose, chunkopts=chunkopts,
				BPPARAM=BPPARAM, ...)
			wts <- Map("*", wts, awts)
		}
	} else {
		wts <- rep_len(weights, length(neighbors))
		weights <- "user-provided weights"
	}
	# calculate global centroid
	if ( is.null(center) ) {
		if ( verbose )
			message("calculating global centroid")
		if ( transpose ) {
			center <- rowStats(x, stat="mean", na.rm=TRUE,
				verbose=FALSE, chunkopts=chunkopts,
				BPPARAM=BPPARAM)
		} else {
			center <- colStats(x, stat="mean", na.rm=TRUE,
				verbose=FALSE, chunkopts=chunkopts,
				BPPARAM=BPPARAM)
		}
	}
	if ( transpose ) {
		distfun <- .spatialColDistFun
	} else {
		distfun <- .spatialRowDistFun
	}
	if ( is.null(bags) ) {
		ans <- nscentroids(x, y=y, s=s, distfun=distfun,
			priors=priors, center=center, transpose=transpose,
			neighbors=neighbors, neighbor.weights=wts,
			verbose=verbose, chunkopts=chunkopts,
			BPPARAM=BPPARAM, ...)
	} else {
		ans <- lapply(s, function(si)
			{
				if ( verbose )
					message("fitting values for s = ", si)
				mi_learn(nscentroids, x=x, y=y,
					s=si, distfun=distfun, bags=bags, score=fitted,
					priors=priors, center=center, transpose=transpose,
					neighbors=neighbors, neighbor.weights=wts,
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
	if ( verbose )
		message("returning shrunken centroids classification")
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
		warning("pending processing steps will be ignored")
	ans <- spatialShrunkenCentroids(spectra(x), y=y,
		coord=coord(x), r=r, s=s, neighbors=neighbors,
		weights=weights, transpose=TRUE, ...)
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
		neighbors = findNeighbors(newdata, r=object$r),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	type <- match.arg(type)
	if ( !missing(newdata) && !is(newdata, "SpectralImagingExperiment") )
		stop("'newdata' must inherit from 'SpectralImagingExperiment'")
	if ( !missing(newdata) ) {
		wts <- spatialWeights(as.matrix(coord(newdata)), neighbors=neighbors)
		if ( object$weights == "adaptive" )
		{
			awts <- spatialWeights(newdata, neighbors=neighbors,
				verbose=FALSE, BPPARAM=BPPARAM, ...)
			wts <- Map("*", wts, awts)
		}
		if ( length(processingData(newdata)) > 0L )
			warning("pending processing steps will be ignored")
		predict(object@model, newdata=spectra(newdata), type=type,
			neighbors=neighbors, neighbor.weights=wts,
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
	if ( "method" %in% ...names() ) {
		.Deprecated(old="method", new="weights")
		weights <- list(...)$method
	}
	if ( is.character(weights) ) {
		weights <- match.arg(weights)
		if ( verbose )
			message("calculating gaussian weights")
		wts <- spatialWeights(as.matrix(coord), neighbors=neighbors)
		if ( weights == "adaptive" )
		{
			if ( verbose )
				message("calculating adaptive weights")
			awts <- spatialWeights(x, neighbors=neighbors,
				weights="adaptive", byrow=!transpose,
				verbose=verbose, chunkopts=chunkopts,
				BPPARAM=BPPARAM, ...)
			wts <- Map("*", wts, awts)
		}
	} else {
		wts <- rep_len(weights, length(neighbors))
		weights <- "custom"
	}
	# calculate global centroid
	if ( is.null(center) ) {
		if ( verbose )
			message("calculating global centroid")
		if ( transpose ) {
			center <- rowStats(x, stat="mean", na.rm=TRUE,
				verbose=FALSE, chunkopts=chunkopts,
				BPPARAM=BPPARAM)
		} else {
			center <- colStats(x, stat="mean", na.rm=TRUE,
				verbose=FALSE, chunkopts=chunkopts,
				BPPARAM=BPPARAM)
		}
	}
	# initialize with k-means
	if ( is.null(init) )
	{
		if ( verbose )
			message("initializing clusters with spatial k-means")
		init <- spatialKMeans(x, k=k, transpose=transpose,
			neighbors=neighbors, weights=wts,
			centers=FALSE, correlation=FALSE,
			verbose=FALSE, chunkopts=chunkopts,
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
			if ( verbose )
				message("fitting k = ", ki, ", s = ", si)
			if ( verbose && nlevels(y) < ki )
				message("starting from ", nlevels(y), " clusters")
			uprop <- 1
			iter <- 1
			while ( iter <= niter && uprop > threshold )
			{
				fit <- spatialShrunkenCentroids(x, y=y, r=r, s=si,
					priors=1, center=center, transpose=transpose,
					neighbors=neighbors, weights=wts,
					verbose=FALSE, chunkopts=chunkopts,
					BPPARAM=BPPARAM, ...)
				utot <- sum(fit$class != y)
				uprop <- utot / length(y)
				if ( verbose )
					message("clustering iteration ", iter, ": ",
						utot, " cluster assignments updated (",
						round(100 * (1 - uprop), digits=2L), "% complete)")
				y <- droplevels(fit$class)
				if ( nlevels(y) < nlevels(fit$class) )
				{
					if ( verbose )
						message("clustering iteration ", iter, ": ",
							"number of clusters dropped from ",
							nlevels(fit$class), " to ", nlevels(y))
					y <- factor(as.integer(y))
				}
				iter <- iter + 1L
			}
			fit$k <- ki
			ans[[i]] <- fit
			i <- i + 1
		}
	}
	# prepare results
	k <- vapply(ans, function(a) a$k, numeric(1L))
	s <- vapply(ans, function(a) a$s, numeric(1L))
	names(ans) <- paste0("r=", r, ",k=", k, ",s=", s)
	if ( verbose )
		message("returning shrunken centroids clustering")
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
		warning("pending processing steps will be ignored")
	ans <- spatialShrunkenCentroids(spectra(x),
		coord=coord(x), r=r, k=k, s=s, neighbors=neighbors,
		weights=weights, transpose=TRUE, ...)
	f <- function(a) as(SpatialResults(a, x), "SpatialShrunkenCentroids")
	if ( is(ans, "ResultsList") ) {
		ResultsList(lapply(ans, f), mcols=mcols(ans))
	} else {
		f(ans)
	}
})



