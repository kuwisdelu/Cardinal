
#### functions for spatially-aware feature-sparse classification ####

calculateNearestSpatialShrunkenCentroids <- function(x, centroids, priors,
	neighbors, alpha, beta, si, s0=median(si)) {
	scores <- calculateSpatialDiscriminantScores(x, # classes=classes,
		centroids=centroids, priors=priors, isNxP=FALSE,
		neighbors=neighbors, alpha=alpha, beta=beta, si=si, s0=s0)
	probabilities <- calculateClassProbabilities(scores)
	classes <- apply(probabilities, 1, which.max)
	list(classes=classes, probabilities=probabilities, scores=scores)
}

calculateClassCentroids <- function(x, classes, isNxP=TRUE) {
	centroids <- sapply(sort(unique(classes)), function(k) {
		is.class <- classes == k
		if ( isNxP ) {
			apply(x[is.class,,drop=FALSE], 2, mean)
		} else {
			apply(x[,is.class,drop=FALSE], 1, mean)
		}
	} )
	centroids
}

calculateClassSizes <- function(classes, na.rm=FALSE) {
	if ( na.rm ) classes <- classes[!is.na(classes)]
	sapply(sort(unique(classes)), function(k) sum(classes == k))
}

calculateWithinClassPooledSD <- function(x, classes, isNxP=TRUE) {
	K <- length(unique(classes))
	wcss <- sqrt(rowSums(calculateFeaturewiseWCSS(x, classes, isNxP)))
	wcss / sqrt((length(classes) - K))
}

calculateFeaturewiseWCSS <- function(x, classes, isNxP=TRUE) {
	wcss <- sapply(sort(unique(classes)), function(k) {
		is.class <- classes == k
		if ( length(is.class) > 1 ) {
			if ( isNxP ) {
				calculateFeaturewiseTSS(x[is.class,,drop=FALSE], isNxP=TRUE)
			} else {
				calculateFeaturewiseTSS(x[,is.class,drop=FALSE], isNxP=FALSE)
			}
		} else {
			0
		}
	} )
	wcss
}

calculateFeaturewiseTSS <- function(x, isNxP=TRUE) {
	if ( isNxP ) {
		colmeans <- apply(x, 2, mean)
		tss <- apply(x, 1, function(xi) (xi - colmeans)^2)
	} else {
		rowmeans <- apply(x, 1, mean)
		tss <- apply(x, 2, function(xi) (xi - rowmeans)^2)
	}
	rowSums(tss)
}

calculateShrunkenCentroids <- function(x, classes, s, isNxP=TRUE,
	si, s0=median(si), probabilities=NULL)
{
	p <- ifelse(isNxP, ncol(x), nrow(x))
	if ( isNxP ) {
		xbari <- apply(x, 2, mean)
	} else {
		xbari <- apply(x, 1, mean)
	}
	if ( is.null(probabilities) ) {
		xbarik <- calculateClassCentroids(x, classes=classes, isNxP=isNxP)
		mk <- sqrt(1 / calculateClassSizes(classes) - 1 / length(classes))
		if ( missing(si) ) si <- calculateWithinClassPooledSD(x, classes=classes,
			isNxP=isNxP)
	} else {
		xbarik <- calculateClassFuzzyCentroids(x, probabilities=probabilities,
			isNxP=isNxP)
		mk <- sqrt(1 / colSums(probabilities) - 1 / nrow(probabilities))
		if ( missing(si) ) si <- calculateWithinClassPooledFuzzySD(x,
			probabilities=probabilities, isNxP=isNxP)
	}
	xdiff <- xbarik - xbari
	se <- rep(mk, each=p) * (si + s0)
	dim(se) <- dim(xbarik)
	dik <- xdiff / se
	d <- soft(dik, s)
	centroids <- xbari + se * d
	list(centroids=centroids, tstatistics=d, sd=si)
}

calculateDiscriminantScores <- function(x, classes, centroids, priors,
	isNxP=TRUE, si, s0=median(si), probabilities=NULL)
{
	p <- ifelse(isNxP, ncol(x), nrow(x))
	if ( missing(si) ) {
		if ( is.null(probabilities) ) {
			si <- calculateWithinClassPooledSD(x, classes=classes, isNxP=isNxP)
		} else {
			si <- calculateWithinClassPooledFuzzySD(x,
				probabilities=probabilities, isNxP=isNxP)
		}
	}
	scores <- apply(centroids, 2, function(xik) {
		if ( isNxP ) {
			apply(x, 1, function(xi) sum((xi - xik)^2 / (si + s0)^2))
		} else {
			apply(x, 2, function(xi) sum((xi - xik)^2 / (si + s0)^2))
		}
	} )
	scores - 2 * log(rep(priors[1:ncol(scores)], each=p))
}

calculateClassProbabilities <- function(scores) {
	means <- rowMeans(scores)
	scores <- scores - means
	probs <- apply(scores, 1, function(d) {
		logp <- -0.5 * d - sum()
		exp(-0.5 * d) / sum(exp(-0.5 * d))
	} )
	t(probs)
}

calculateSpatialDiscriminantScores <- function(x, classes, centroids, priors,
	isNxP=TRUE, neighbors, alpha, beta, si, s0=median(si), .C=TRUE)
{
	p <- ifelse(isNxP, ncol(x), nrow(x))
	n <- ifelse(isNxP, nrow(x), ncol(x))
	if ( missing(si) ) si <- calculateWithinClassPooledSD(x, classes, isNxP)
	sd <- si + s0
	if ( .C ) {
		if ( isNxP ) {
			x <- t(as.matrix(x))
		} else {
			x <- as.matrix(x)
		}
		w <- rep(1, p)
		scores <- matrix(0, nrow=n, ncol=ncol(centroids))
		scores <- .C("discriminant_scores_spatial", as.double(x), as.integer(nrow(x)),
			as.integer(ncol(x)), as.integer(neighbors - 1), as.integer(ncol(neighbors)),
			as.double(w), as.double(alpha), as.double(beta), as.double(centroids),
			as.integer(ncol(centroids)), as.double(sd), as.double(scores))[[12]]
		dim(scores) <- c(n, ncol(centroids))
	} else {
		scores <- sapply(seq_len(n), function(i) {
			gamma <- alpha * beta[i,]
			gamma <- gamma / sum(gamma)
			gamma <- rep(gamma, each=p)
			if ( isNxP ) {
				xi <- t(x[neighbors[i,],,drop=FALSE])
			} else {
				xi <- x[,neighbors[i,],drop=FALSE]
			}
			apply(centroids, 2, function(xik) sum(gamma * (xi - xik)^2 / sd^2))
		} )
		scores <- t(scores)
	}
	scores - 2 * log(rep(priors[1:ncol(scores)], each=n))
}

calculateClassFuzzyCentroids <- function(x, probabilities, isNxP=TRUE) {
	centroids <- apply(probabilities, 2, function(pr) {
		if ( isNxP ) {
			apply(x, 2, function(xi) sum(xi * pr)) / sum(pr)
		} else {
			apply(x, 1, function(xi) sum(xi * pr)) / sum(pr)
		}
	} )
	centroids
}

calculateWithinClassPooledFuzzySD <- function(x, probabilities, isNxP=TRUE) {
	K <- ncol(probabilities)
	wcss <- sqrt(rowSums(calculateFeaturewiseFuzzyWCSS(x, probabilities, isNxP)))
	wcss / sqrt((nrow(probabilities) - K))
}

calculateFeaturewiseFuzzyWCSS <- function(x, probabilities, isNxP=TRUE) {
	centroids <- calculateClassFuzzyCentroids(x, probabilities=probabilities,
		isNxP=isNxP)
	wcss <- sapply(1:ncol(centroids), function(k) {
		if ( isNxP ) {
			sapply(1:ncol(x), function(i) sum(probabilities[,k] *
				(x[,i] - centroids[i,k])^2))
		} else {
			sapply(1:nrow(x), function(i) sum(probabilities[,k] *
				(x[i,] - centroids[i,k])^2))
		}
	} )
	wcss
}

