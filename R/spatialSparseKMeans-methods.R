
#### implement methods for spatially-aware feature-sparse clustering ####

setMethod("spatialSparseKMeans", "MSImageSet", function(object, r, k, s,
	method=c("gaussian", "adaptive"), ncomp=20, standardize=TRUE,
	neighbors, w, alpha, beta, autoDimNames=NULL, initialize=NULL,
	iter.max=10, nstart=1, tol=1e-4, ...)
{
	method <- match.arg(method)
	if ( any(k < 2) ) stop("'k' must be greater than or equal to 2")
	# initialize any unspecified parameters
	if ( is.list(initialize) ) {
		if ( length(initialize) != length(k) ) {
			stop("'initialize' is shorter than length of 'k'")
		}
	} else if ( !is.null(initialize) ) {
		initialize <- rep(list(initialize), length(k))
	}
	if ( missing(neighbors) ) {
		tryVerboseMessage("Finding neighbors...", precedes.progress.output=FALSE)
		neighbors <- NULL # necessary to make neighbors() work
		neighbors <- neighbors(object, r=r, autoDimNames=autoDimNames, na.replace=TRUE)
	}
	if ( missing(w) ) w <- rep(1 / sqrt(numFeatures(object)), numFeatures(object))
	if ( missing(alpha) ) alpha <- spatial.alpha(r=r, p=ncol(coord(object)) - length(autoDimNames))
	if ( missing(beta) ) {
		tryVerboseMessage("Calculating spatial weights...", precedes.progress.output=FALSE)
		if ( method == "gaussian" ) {
			beta <- matrix(1, nrow=nrow(neighbors), ncol=ncol(neighbors))
		} else if ( method == "adaptive" ) {
			beta <- spatial.beta(object@spectra$spectra, neighbors=neighbors)
		} else {
			stop("method ", method, " not recognized")
		}
	}
	# form the concatenated spectra
	tryVerboseMessage("Concatenating neighborhood mass spectra...", precedes.progress.output=FALSE)
	concatSpectra <- concatenateSpectra(object@spectra$spectra, standardize, neighbors,
			alpha, beta)
	# prepare the data for clustering and the output statistics
	featurewiseTSS <- calculateConcatenatedTSS(concatSpectra, numFeatures(object))
	TSS <- calculateTSS(object@spectra$spectra)
	k <- sort(k)
	s <- sort(s)
	# get initial clusters from ordinary spatially-aware clustering sequentially
	tryVerboseMessage("Initializing...", precedes.progress.output=FALSE)
	seeding <- spatialKMeans(object, r=r, k=k, method=method, ncomp=ncomp,
		standardize=standardize, neighbors=neighbors, w=w, alpha=alpha, beta=beta,
		autoDimNames=autoDimNames, initialize=initialize, iter.max=iter.max,
		nstart=nstart, ...)
	clusters <- rep(seeding$clusters, each=length(s))
	par <- expand.grid(s=s, k=k)
	# do the spatially-aware feature-sparse clustering
	tryVerboseMessage("Performing sparse clustering...", precedes.progress.output=FALSE)
	results <- mapply(function(ki, si, clustersi) {
		tryVerboseMessage("r = ", r, ", k = ", ki, ", s = ", si, "...", precedes.progress.output=FALSE)
		spatialSparseKMeansWarmStart(object@spectra$spectra, concatSpectra,
			clusters=clustersi, k=ki, s=si, featurewiseTSS=featurewiseTSS,
			iter.max=iter.max, nstart=nstart, neighbors=neighbors,
			w=w, tol=tol)
	}, par$k, par$s, clusters, SIMPLIFY=FALSE)
	names(results) <- paste("k = ", sapply(results, function(x) x$k),
		", s = ", sapply(results, function(x) x$s), sep="")
	outlist <- list(clusters=lapply(results, function(x) x$clusters),
		centers=sapply(results, function(x) x$centers, USE.NAMES=TRUE),
		sizes=sapply(results, function(x) x$sizes, USE.NAMES=TRUE),
		WCSS=sapply(results, function(x) x$WCSS, USE.NAMES=TRUE),
		BCSS=sapply(results, function(x) TSS - x$WCSS, USE.NAMES=TRUE),
		weights=sapply(results, function(x) x$w, USE.NAMES=TRUE))
	names(outlist$clusters) <- names(results)
	outlist$fastmap <- seeding$fastmap
	class(outlist) <- "KMeans"
	return(outlist)
} )

