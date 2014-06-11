
setMethod("spatialKMeans",
	signature = c(x = "SImageSet"),
	function(x, r = 1, k = 2,
		method = c("gaussian", "adaptive"),
		weights = 1, iter.max = 10, nstart = 100, ...)
{
	method <- match.arg(method)
	save.seed()
	rs <- sort(r)
	ks <- sort(k)
	w <- rep(weights, length.out=nrow(iData(x)))
	out <- unlist(lapply(rs, function(r){
		spatial <- .calculateSpatial(x, r=r)
		fastmap <- fastmap.spatial(iData(x), r=r, spatial=spatial, w=weights, ...)
		lapply(ks, function(k) {
			append(.spatialKMeans(x, fastmap=fastmap, k=k, iter.max=iter.max, nstart=nstart, ...),
				list(r=r, k=k, method=method, weights=weights))
		})
	}), recursive=FALSE)
	par <- AnnotatedDataFrame(data=data.frame(
			k=sapply(out, function(fit) fit$r),
			s=sapply(out, function(fit) fit$s)),
		varMetadata=data.frame(
			labelDescription=c(
				k="Number of clusters",
				r="Neighborhood radius")))
	featureNames(par) <- formatParam(pData(par))
	names(out) <- formatParam(pData(par))
	new("SpatialKMeans",
		pixelData=x@pixelData,
		featureData=x@featureData,
		experimentData=x@experimentData,
		protocolData=x@protocolData,
		resultData=out,
		modelData=par)
})

.spatialKMeans <- function(x, fastmap, k, iter.max, nstart, ...) {
	restore.seed()
	cluster <- kmeans(fastmap$scores, centers=k,
		iter.max=iter.max, nstart=nstart)$cluster
	centers <- sapply(sort(unique(cluster)), function(Ck) {
		rowMeans(iData(x)[,cluster==Ck,drop=FALSE])
	})
	list(cluster=cluster, centers=centers)
}

.calculateSpatial <- function(x, r) {
	neighbors <- generateNeighbors(coord(x), r=r, na.rm=TRUE)
	alpha <- spatial.alpha(r=r, p=length(coord(x)))
	if ( method == "adaptive" ) {
		beta <- spatial.beta(iData(x), neighbors)
	} else {
		beta <- rep(list(matrix(1, nrow=nrow(alpha), ncol=nrow(alpha))), ncol(iData(x)))
	}
	list(neighbors=neighbors, alpha=alpha, beta=beta)
}

spatial.alpha <- function(r, p) {
	sigma <- ((2 * r) + 1) / 4
	radii <- lapply(1:p, function(i) (-r):r)
	neighborhood <- expand.grid(radii)
	alpha <- apply(neighborhood, 1, function(i) exp((-sum(i^2))/(2 * sigma^2)))
	dim(alpha) <- rep((2 * r) + 1, p)
	alpha
}

spatial.beta <- function(x, neighbors) {
	if ( length(neighbors[[1]]) == 1 ) {
		matrix(1, ncol=length(neighbors))
	} else {
		mapply(function(i, nb) {
			delta <- sqrt(colSums((x[,i] - x[,nb,drop=FALSE])^2))
			lambda <- max(delta - min(delta)) / 2
			lambda <- ifelse(lambda > 0, lambda, 1)
			beta <- exp(-delta^2 / (2 * lambda^2))
			dim(beta) <- dim(nb)
			dimnames(beta) <- dimnames(nb)
			beta
		}, seq_len(ncol(x)), neighbors, SIMPLIFY=FALSE)
	}
}
