
#### functions for spatialSparseKMeans ####

spatialSparseKMeansWarmStart <- function(x, xconcat, clusters, k, s,
	featurewiseTSS, iter.max, nstart, neighbors, w, tol)
{
	p <- length(w)
	clusters.last <- sample(clusters)
	wcss <- calculateConcatenatedWCSS(xconcat, p, clusters)
	tss <- featurewiseTSS
	bcss <- tss - rowSums(wcss)
	# lambda <- choose.lambda(bcss, s)
	lambda <- s # use raw penalty
	w.last <- w
	w <- choose.w(bcss, choose.delta(bcss, lambda))
	iter <- 1
	tryVerboseMessage("+", appendLF=FALSE, console.only=TRUE, precedes.progress.output=FALSE)
	while ( iter < iter.max && any(clusters.last != clusters) && l2norm(w - w.last) > tol ) {
		w.last <- w
		clusters.last <- clusters
		nonzero <- rep(w > 0, nrow(xconcat) / p)
		wxconcat <- w[w > 0] * xconcat[nonzero,]
		centers <- calculateClusterCenters(wxconcat, clusters, isNxP=FALSE)
		suppressWarnings(clusters <- kmeans(t(wxconcat), centers=t(centers),
			iter.max=iter.max, nstart=1)$cluster)
		if ( length(unique(clusters)) < k ) {
			warning("empty cluster; restarting from random configuration")
			clusters <- kmeans(t(wxconcat), centers=k, iter.max=iter.max,
				nstart=nstart)$cluster
		}
		wcss <- calculateConcatenatedWCSS(xconcat, p, clusters)
		bcss <- tss - rowSums(wcss)
		# lambda <- choose.lambda(bcss, s)
		lambda <- s # use raw penalty
		w <- choose.w(bcss, choose.delta(bcss, lambda))
		tryVerboseMessage(" ", iter, appendLF=FALSE, console.only=TRUE, precedes.progress.output=FALSE)
		iter <- iter + 1
	}
	tryVerboseMessage("", console.only=TRUE, precedes.progress.output=FALSE)
	if ( any(clusters.last != clusters) | l2norm(w - w.last) > 1e-6 ) {
		warning("spatially-aware feature-sparse k-means did not converge in ", iter.max, " iterations")
	}
	centers <- calculateClusterCenters(x, clusters, isNxP=FALSE)
	sizes <- tabulate(clusters)
	wcss <- calculateWCSS(x, clusters, isNxP=FALSE)
	list(clusters=clusters, centers=centers, sizes=sizes, WCSS=wcss,
		k=k, s=s, w=w)
}

concatenateSpectra <- function(x, standardize, neighbors, alpha, beta) {
	if ( missing(alpha) ) alpha <- rep(1, nrow(neighbors))
	if ( missing(beta) ) beta <- matrix(1, nrow=nrow(neighbors), ncol=ncol(neighbors))
	if ( standardize ) x <- t(apply(x, 1, scale))
	xconcat <- matrix(nrow=nrow(x) * ncol(neighbors), ncol=ncol(x))
	for ( i in 1:ncol(x) ) {
		xi <- as.numeric(x[,neighbors[i,]])
		gamma <- sqrt(alpha) * sqrt(beta[i,])
		gamma <- gamma / sum(gamma)
		gamma <- rep(gamma, each=nrow(x))
		xconcat[,i] <- gamma * xi
	}
	xconcat
}

calculateConcatenatedTSS <- function(xconcat, p) {
	if ( nrow(xconcat) %% p != 0 ) stop("# of rows in 'x' is not a multiple of 'p'")
	rowmeans <- apply(xconcat, 1, mean)
	tss <- sapply(1:nrow(xconcat), function(i) sum((xconcat[i,] - rowmeans[i])^2))
	tss <- matrix(tss, nrow=p)
	rowSums(tss)
}

calculateConcatenatedWCSS <- function(xconcat, p, clusters) {
	if ( nrow(xconcat) %% p != 0 ) stop("# of rows in 'x' is not a multiple of 'p'")
	wcss <- sapply(sort(unique(clusters)), function(k) {
		is.cluster <- clusters == k
		if ( length(is.cluster) > 1 ) {
			return(calculateConcatenatedTSS(xconcat[,is.cluster,drop=FALSE], p=p))
		} else {
			return(0)
		}
	} )
	wcss
}

choose.w <- function(a, delta) {
	s <- soft(a, delta)
	s / l2norm(s)
}

choose.delta <- function(a, s, iter.max=10, epsilon=1e-9) {
	if ( l1norm(a) < epsilon | l1norm(a / l2norm(a)) <= s ) return(0)
	lo <- 0
	hi <- max(abs(a)) - epsilon
	delta <- (lo + hi) / 2
	iter <- 1
	while ( iter <= iter.max & l2norm(l1norm(choose.w(a, delta)) - s) > epsilon ) {
		if ( l1norm(choose.w(a, delta)) > s ) {
			lo <- delta
		} else {
			hi <- delta
		}
		delta <- (lo + hi) / 2
		iter <- iter + 1
	}
	delta
}

choose.lambda <- function(a, s, iter.max=20, epsilon=1e-3) {
	if ( l1norm(a) < epsilon ) return(0)
	lo <- 0
	hi <- l1norm(choose.w(a, 0))
	if ( s <= 0 ) {
		return(lo)
	} else if ( s >= 1 ) {
		return(hi)
	}
	lambda <- (lo + hi) / 2
	ws <- sum(choose.w(a, choose.delta(a, lambda)) > 0) / length(a)
	iter <- 1
	while ( iter <= iter.max & l2norm(ws - s) > epsilon ) {
		if ( ws > s ) {
			hi <- lambda
		} else {
			lo <- lambda
		}
		lambda <- (lo + hi) / 2
		ws <- sum(choose.w(a, choose.delta(a, lambda)) > 0) / length(a)
		iter <- iter + 1
	}
	lambda
}

