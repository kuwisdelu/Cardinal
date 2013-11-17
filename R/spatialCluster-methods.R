

#### implement shorthand for spatially-aware clustering with s = 0 ####

setMethod("spatialCluster", "MSImageSet", function(object, r, k,
	method=c("gaussian", "adaptive"), autoDimNames=NULL, iter.max=10,
	tol=1e-4, ...)
{
	outlist <- spatialSparseCluster(object, r=r, k=k, s=0, method=method,
		autoDimNames=autoDimNames, iter.max=iter.max, tol=tol, ...)
	return(outlist)
} )

#### implement methods for spatially-aware feature-sparse clustering ####

setMethod("spatialSparseCluster", "MSImageSet", function(object, r, k, s,
	method=c("gaussian", "adaptive"), autoDimNames=NULL, iter.max=10,
	tol=1e-4, ...)
{
	method <- match.arg(method)
	priors <- lapply(k, function(ki) rep(1/ki, ki))
	if ( any(k < 2) ) stop("'k' must be greater than or equal to 2")
	if ( length(priors) != length(k) ) stop("'priors' must be given for all 'k'")
	# initialize variables
	tryVerboseMessage("Finding neighbors...", precedes.progress.output=FALSE)
	neighbors <- neighbors(object, r=r, autoDimNames=autoDimNames, na.replace=TRUE)
	tryVerboseMessage("Calculating spatial weights...", precedes.progress.output=FALSE)
	alpha <- spatial.alpha(r=r, p=ncol(coord(object)) - length(autoDimNames))
	if ( method == "gaussian" ) {
		beta <- matrix(1, nrow=nrow(neighbors), ncol=ncol(neighbors))
	} else if ( method == "adaptive" ) {
		beta <- spatial.beta(object@spectra$spectra, neighbors=neighbors)
	} else {
		stop("method ", method, " not recognized")
	}
	# prepare for clustering
	k <- sort(k)
	s <- sort(s)
	# get initial clusters from ordinary spatially-aware clustering sequentially
	tryVerboseMessage("Initializing...", precedes.progress.output=FALSE)
	initkmeans <- spatialKMeans(object, r=r, k=k, method=method, neighbors=neighbors, 
		alpha=alpha, beta=beta, autoDimNames=autoDimNames, ...)
	initialize <- rep(initkmeans$clusters, each=length(s))
	priors <- rep(priors, each=length(s))
	par <- expand.grid(s=s, k=k)
	# do the feature-sparse clustering
	tryVerboseMessage("Performing shrunken centroid clustering...", precedes.progress.output=FALSE)
	results <- mapply(function(ki, si, initializei, priorsi) {
		tryVerboseMessage("r = ", r, ", k = ", ki, ", s = ", si, "...", precedes.progress.output=FALSE)
		spatialSparseClusterWarmStart(object@spectra$spectra, k=ki, s=si,
			priors=priorsi, initialize=initializei, neighbors=neighbors,
			alpha=alpha, beta=beta, iter.max=iter.max, tol=tol)
	}, par$k, par$s, initialize, priors, SIMPLIFY=FALSE)
	# aggregate the results and return them
	names(results) <- paste("r = ", r, ", k = ", sapply(results, function(x) x$k),
		", s = ", sapply(results, function(x) x$s), sep="")
	outlist <- list(classes=sapply(results, function(x) x$classes, simplify=FALSE, USE.NAMES=TRUE),
		probabilities=sapply(results, function(x) x$probabilities, simplify=FALSE, USE.NAMES=TRUE),
		scores=sapply(results, function(x) x$scores, simplify=FALSE, USE.NAMES=TRUE),
		centroids=sapply(results, function(x) x$centroids, simplify=FALSE, USE.NAMES=TRUE),
		tstatistics=sapply(results, function(x) x$tstatistics, simplify=FALSE, USE.NAMES=TRUE),
		sd=sapply(results, function(x) x$sd, simplify=FALSE, USE.NAMES=TRUE),
		dof=sapply(results, function(x) sum(abs(x$tstatistics) > 0) + numFeatures(object), USE.NAMES=TRUE),
		nclasses=sapply(results, function(x) ncol(x$centroids), USE.NAMES=TRUE),
		nfeatures=sapply(results, function(x) apply(x$tstatistics, 2, function(t) sum(abs(t) > 0)), simplify=FALSE, USE.NAMES=TRUE))
	outlist$priors <- priors
	names(outlist$priors) <- names(results)
	outlist$fastmap <- initkmeans$fastmap
	metaData <- list(name=metaData(object)[["name"]], mz=mz(object),
		coord=coord(object), r=r, method=method, autoDimNames=autoDimNames,
		levels=1:max(k), labels=paste(1:max(k)), isfactor=FALSE,
		parnames=names(results), npar=length(results))
	metaData$parameters <- data.frame(r=r, k=sapply(results, function(x) x$k),
		s=sapply(results, function(x) x$s))
	fit <- new("MSImageSegmentation", .Data=outlist, metaData=metaData)
	attr(fit, "names") <- names(outlist)
	# return the fitted segmentation
	fit <- tryCatch(calibrateClasses(fit), error=function(e) {
		warning("calibration of clusters between parameter sets failed")
		fit
	} )
	return(fit)
} )

