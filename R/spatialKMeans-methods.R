
# #### implement methods for spatially-aware clustering ####

# setMethod("spatialKMeans", "MSImageSet", function(object, r, k,
# 	method=c("gaussian", "adaptive"), ncomp=20, standardize=TRUE,
# 	neighbors, w, alpha, beta, autoDimNames=NULL, initialize=NULL,
# 	iter.max=10, nstart=1, ...)
# {
# 	method <- match.arg(method)
# 	if ( !exists(".Random.seed", envir=globalenv()) ) set.seed(k[[1]])
# 	.Last.Random.seed <- get(".Random.seed", envir=globalenv())
# 	# initialize any unspecified parameters
# 	if ( is.list(initialize) ) {
# 		if ( length(initialize) != length(k) ) {
# 			stop("'initialize' is shorter than length of 'k'")
# 		}
# 	} else if ( !is.null(initialize) ) {
# 		initialize <- rep(list(initialize), length(k))
# 	}
# 	if ( missing(neighbors) ) {
# 		tryVerboseMessage("Finding neighbors...", precedes.progress.output=FALSE)
# 		neighbors <- NULL # necessary to make neighbors() work
# 		neighbors <- neighbors(object, r=r, autoDimNames=autoDimNames, na.replace=TRUE)
# 	}
# 	if ( missing(w) ) w <- rep(1 / sqrt(numFeatures(object)), numFeatures(object))
# 	if ( missing(alpha) ) alpha <- spatial.alpha(r=r, p=ncol(coord(object)) - length(autoDimNames))
# 	if ( missing(beta) ) {
# 		tryVerboseMessage("Calculating spatial weights...", precedes.progress.output=FALSE)
# 		if ( method == "gaussian" ) {
# 			beta <- matrix(1, nrow=nrow(neighbors), ncol=ncol(neighbors))
# 		} else if ( method == "adaptive" ) {
# 			beta <- spatial.beta(object@spectra$spectra, neighbors=neighbors)
# 		} else {
# 			stop("method ", method, " not recognized")
# 		}
# 	}
# 	# perform the Fastmap projection of spectra using spatially-embedded distances
# 	tryVerboseMessage("Projecting data via FastMap...", precedes.progress.output=FALSE)
# 	fastmap <- fastmap.spatial(object@spectra$spectra, r=r, ncomp=ncomp, standardize=standardize,
# 			neighbors=neighbors, w=w, alpha=alpha, beta=beta)
# 	# prepare the data for clustering and the output statistics
# 	TSS <- calculateTSS(object@spectra$spectra)
# 	k <- sort(k)
# 	coord <- force(coord(object))
# 	# do spatially-aware clustering
# 	tryVerboseMessage("Clustering embedded spatial data...", precedes.progress.output=FALSE)
# 	results <- lapply(seq_along(k), function(i) {
# 		tryVerboseMessage("r = ", r, ", k = ", k[[i]], precedes.progress.output=FALSE)
# 		spatialKMeansWarmStart(object@spectra$spectra, fastmap$scores,
# 			clusters=initialize[[i]], k=k[[i]], iter.max=iter.max,
# 			nstart=nstart, .Last.Random.seed=.Last.Random.seed)
# 	} )
# 	names(results) <- paste("k =", sapply(results, function(x) x$k))
# 	outlist <- list(clusters=lapply(results, function(x) x$clusters),
# 		centers=sapply(results, function(x) x$centers, USE.NAMES=TRUE),
# 		sizes=sapply(results, function(x) x$sizes, USE.NAMES=TRUE),
# 		WCSS=sapply(results, function(x) x$WCSS, USE.NAMES=TRUE),
# 		BCSS=sapply(results, function(x) TSS - x$WCSS, USE.NAMES=TRUE))
# 	names(outlist$clusters) <- names(results)
# 	outlist$fastmap <- fastmap
# 	class(outlist) <- "KMeans"
# 	return(outlist)
# } )
