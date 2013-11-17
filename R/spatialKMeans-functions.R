
#### functions for spatialKMeans ####

spatialKMeansWarmStart <- function(x, projection, clusters, k,
	iter.max, nstart, .Last.Random.seed)
{
	if ( is.null(clusters) ) {
		initcenters <- k
	} else {
		initcenters <- initializeSeededClusters(x, clusters, k, isNxP=FALSE)
	}
	.Random.seed <<- .Last.Random.seed
	clusters <- kmeans(projection, centers=initcenters, iter.max=iter.max,
		nstart=nstart)$cluster
	centers <- calculateClusterCenters(x, clusters, isNxP=FALSE)
	sizes <- tabulate(clusters)
	wcss <- calculateWCSS(x, clusters, isNxP=FALSE)
	list(clusters=clusters, centers=centers, sizes=sizes, WCSS=wcss, k=k)
}

spatial.alpha <- function(r, p=2) {
	sigma <- ((2*r+1)/4)
	rs <- lapply(1:p, function(i) (-r):r)
	neighborhood <- expand.grid(rs)
	alpha <- apply(neighborhood, 1, function(i) exp((-sum(i^2))/(2*sigma^2)))
	alpha
}

spatial.beta <- function(x, neighbors) {
	if ( ncol(neighbors) == 1 ) return(as.matrix(rep(1, nrow(neighbors))))
	self <- (col(neighbors) + 1) / 2
	beta <- sapply(1:ncol(x), function(i) {
		delta <- sqrt(colSums((x[,i] - x[,neighbors[i,],drop=FALSE])^2))
		lambda <- max(delta - min(delta)) / 2
		lambda <- ifelse(lambda > 0, lambda, 1)
		exp(-delta^2 / (2 * lambda^2))
	} )
	t(beta)
}

initializeSeededClusters <- function(x, clusters, k, isNxP=TRUE) {
	ninit <- length(unique(clusters))
	if ( ninit > k ) {
		stop("number of seeded clusters is greater than 'k'")
	} else if ( ninit < k ) {
		warning("number of seeded clusters is less than 'k'")
	}
	calculateClusterCenters(x, clusters=clusters, isNxP=isNxP)
}

calculateClusterCenters <- function(x, clusters, isNxP=TRUE) {
	centers <- sapply(sort(unique(clusters)), function(k) {
		is.cluster <- clusters == k
		if ( isNxP ) {
			colMeans(x[is.cluster,,drop=FALSE])
		} else {
			rowMeans(x[,is.cluster,drop=FALSE])
		}
	} )
	centers
}

calculateTSS <- function(x, isNxP=TRUE) {
	if ( isNxP ) {
		colmeans <- apply(x, 2, mean)
		tss <- apply(x, 1, function(xi) sum((xi - colmeans)^2))
	} else {
		rowmeans <- apply(x, 1, mean)
		tss <- apply(x, 2, function(xi) sum((xi - rowmeans)^2))
	}
	sum(tss)
}

calculateWCSS <- function(x, clusters, isNxP=TRUE) {
	wcss <- sapply(sort(unique(clusters)), function(k) {
		is.cluster <- clusters == k
		if ( length(is.cluster) < 1 ) return(0)
		if ( isNxP ) {
			return(calculateTSS(x[is.cluster,,drop=FALSE], isNxP=TRUE))	
		} else {
			return(calculateTSS(x[,is.cluster,drop=FALSE], isNxP=FALSE))	
		}
	} )
	wcss
}

