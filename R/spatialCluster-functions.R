
#### functions for spatially-aware feature-sparse clustering ####

spatialSparseClusterWarmStart <- function(x, k, s, priors, initialize,
	neighbors, alpha, beta, iter.max, tol, fuzzy)
{
	classes <- initialize
	centroids <- x[,1:k]
	probabilities <- calculateClassMasks(classes)
	classes.last <- rep(0, length(classes))
	centroids.last <- matrix(0, nrow=nrow(x), ncol=k)
	iter <- 1
	tryVerboseMessage("+", appendLF=FALSE, console.only=TRUE, precedes.progress.output=FALSE)
	while ( any(classes != classes.last) && l2norm(centroids - centroids.last) > tol && iter <= iter.max ) {
		classes.last <- classes
		centroids.last <- centroids
		shrinkage <- calculateShrunkenCentroids(x, s=s, isNxP=FALSE, classes=classes)
		degenerates <- (apply(shrinkage$tstatistics, 2, function(t) sum(abs(t) > 0)) == 0)
		if ( any(degenerates) ) {
			iter <- 1
			tryVerboseMessage("\n+", appendLF=FALSE, console.only=TRUE, precedes.progress.output=FALSE)
			shrinkage$centroids <- shrinkage$centroids[,!degenerates]
			centroids.last <- centroids.last[,!degenerates]
		}
		scores <- calculateSpatialDiscriminantScores(x, classes=classes,
			centroids=shrinkage$centroids, priors=priors, isNxP=FALSE,
			neighbors=neighbors, alpha=alpha, beta=beta, si=shrinkage$sd)
		probabilities <- calculateClassProbabilities(scores)
		classes <- apply(probabilities, 1, which.max)
		centroids <- shrinkage$centroids
		if ( ncol(centroids) != ncol(centroids.last) ) {
			iter <- 1
			tryVerboseMessage("\n+", appendLF=FALSE, console.only=TRUE, precedes.progress.output=FALSE)
			centroids.last <- centroids.last[,1:ncol(centroids)]
		}
		tryVerboseMessage(" ", iter, appendLF=FALSE, console.only=TRUE, precedes.progress.output=FALSE)
		iter <- iter + 1
	}
	tryVerboseMessage("", console.only=TRUE, precedes.progress.output=FALSE)
	if ( any(classes.last != classes) && l2norm(centroids - centroids.last) > tol ) {
		warning("spatially-aware feature-sparse clustering did not converge in ", iter.max, " iterations")
	}
	list(classes=classes, probabilities=probabilities, scores=scores, k=k, s=s,
		centroids=shrinkage$centroids, tstatistics=shrinkage$tstatistics,
		sd=shrinkage$sd)
}

calculateClassMasks <- function(classes) {
	Ck <- sort(unique(classes))
	sapply(Ck, function(i) as.numeric(classes == i))
}

