
#### Spatially-aware shrunken centroids ####
## -----------------------------------------

setMethod("spatialShrunkenCentroids", c(x = "ANY", y = "ANY"),
	function(x, y, coord, r = 1, s = 0,
		weights = c("gaussian", "adaptive"),
		neighbors = findNeighbors(coord, r=r),
		priors = table(y), center = NULL, transpose = FALSE,
		nchunks = getCardinalNChunks(),
		verbose = getCardinalVerbose(),
		BPPARAM = getCardinalBPPARAM(), ...)
{
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
				nchunks=nchunks, verbose=verbose,
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
				nchunks=nchunks, verbose=FALSE,
				BPPARAM=BPPARAM)
		} else {
			center <- colStats(x, stat="mean", na.rm=TRUE,
				nchunks=nchunks, verbose=FALSE,
				BPPARAM=BPPARAM)
		}
	}
	if ( transpose ) {
		distfun <- .spatialColDistFun
	} else {
		distfun <- .spatialRowDistFun
	}
	ans <- nscentroids(x, y=y, s=s, distfun=distfun,
		priors=priors, center=center, transpose=transpose,
		neighbors=neighbors, neighbor.weights=wts,
		nchunks=nchunks, verbose=verbose,
		BPPARAM=BPPARAM, ...)
	if ( is(ans, "nscentroids") )
		ans <- list(ans)
	ans <- lapply(ans,
		function(a) {
			a$weights <- weights
			a$r <- r
			a
		})
	if ( verbose )
		message("returning shrunken centroids classification")
	if ( length(ans) > 1L ) {
		mcols <- DataFrame(r=r, s=s, weights=weights)
		ResultsList(ans, mcols=mcols)
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
	ans <- object$probability
	if ( type == "class" ) {
		cls <- apply(ans, 1L, which.max)
		ans <- factor(cls,
			levels=seq_len(ncol(ans)),
			labels=colnames(ans))
	}
	ans
})

setMethod("predict", "SpatialShrunkenCentroids",
	function(object, newdata,
		type = c("response", "class"),
		neighbors = findNeighbors(newdata, r=object$r),
		nchunks = getCardinalNChunks(),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( !missing(newdata) && !is(newdata, "SpectralImagingExperiment") )
		stop("'newdata' must inherit from 'SpectralImagingExperiment'")
	if ( !missing(newdata) ) {
		wts <- spatialWeights(as.matrix(coord(newdata)), neighbors=neighbors)
		if ( object$weights == "adaptive" )
		{
			awts <- spatialWeights(newdata, neighbors=neighbors,
				nchunks=nchunks, verbose=FALSE,
				BPPARAM=BPPARAM, ...)
			wts <- Map("*", wts, awts)
		}
		if ( length(processingData(newdata)) > 0L )
			warning("pending processing steps will be ignored")
		predict(object@model, newdata=spectra(newdata), type=type,
			neighbors=neighbors, neighbor.weights=wts,
			nchunks=nchunks, verbose=FALSE,
			BPPARAM=BPPARAM, ...)
	} else {
		fitted(object@model, type=type, ...)
	}
})

setMethod("spatialShrunkenCentroids", c(x = "ANY", y = "missing"),
	function(x, coord, r = 1, k = 2, s = 0,
		weights = c("gaussian", "adaptive"),
		neighbors = findNeighbors(coord, r=r),
		init = NULL, threshold = 0.01, niter = 10L,
		center = NULL, transpose = FALSE,
		nchunks = getCardinalNChunks(),
		verbose = getCardinalVerbose(),
		BPPARAM = getCardinalBPPARAM(), ...)
{
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
				nchunks=nchunks, verbose=verbose,
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
				nchunks=nchunks, verbose=FALSE,
				BPPARAM=BPPARAM)
		} else {
			center <- colStats(x, stat="mean", na.rm=TRUE,
				nchunks=nchunks, verbose=FALSE,
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
			nchunks=nchunks, verbose=FALSE,
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
			# perform segmentation
			if ( verbose )
				message("fitting k = ", init[[j]]$k, ", s = ", si)
			if ( is.null(y) )
				y <- factor(init[[j]]$cluster)
			uprop <- 1
			iter <- 1
			while ( iter <= niter && uprop > threshold )
			{
				fit <- spatialShrunkenCentroids(x, y=y, r=r, s=si,
					priors=1, center=center, transpose=transpose,
					neighbors=neighbors, weights=wts,
					nchunks=nchunks, verbose=FALSE,
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
			fit$k <- init[[j]]$k
			ans[[i]] <- fit
			i <- i + 1
		}
	}
	if ( verbose )
		message("returning shrunken centroids clustering")
	if ( length(ans) > 1L ) {
		s <- vapply(ans, function(a) a$s, numeric(1L))
		k <- vapply(ans, function(a) a$k, numeric(1L))
		mcols <- DataFrame(r=r, k=k, s=s, weights=weights)
		ResultsList(ans, mcols=mcols)
	} else {
		ans[[1L]]
	}
})

setMethod("spatialShrunkenCentroids", c(x = "SpectralImagingExperiment", y = "missing"),
	function(x, y, r = 1, k = 2, s = 0,
		weights = c("gaussian", "adaptive"),
		neighbors = findNeighbors(x, r=r), ...)
{
	if ( length(processingData(x)) > 0L )
		warning("pending processing steps will be ignored")
	ans <- spatialShrunkenCentroids(spectra(x),
		coord=coord(x), r=r, k=k, s=s, neighbors=neighbors,
		weights=weights, transpose=TRUE, ...)
	if ( is(ans, "ResultsList") ) {
		f <- function(a) as(SpatialResults(a, x), "SpatialShrunkenCentroids")
		ResultsList(lapply(ans, f), mcols=mcols(ans))
	} else {
		as(SpatialResults(ans, x), "SpatialShrunkenCentroids")
	}
})

