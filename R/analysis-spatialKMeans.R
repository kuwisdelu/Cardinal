
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
		spatial <- .calculateSpatialInfo(x, r=r, method=method)
		fastmap <- fastmap.spatial(iData(x), r=r, spatial=spatial, w=w, ...)
		lapply(ks, function(k) {
			append(.spatialKMeans(x, fastmap=fastmap, k=k, iter.max=iter.max, nstart=nstart, ...),
				list(r=r, k=k, method=method, weights=weights))
		})
	}), recursive=FALSE)
	par <- AnnotatedDataFrame(data=data.frame(
			r=sapply(out, function(fit) fit$r),
			k=sapply(out, function(fit) fit$k)),
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

.calculateSpatialInfo <- function(x, r, method) {
	neighbors <- spatial.neighbors(x, r=r, na.rm=TRUE)
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

spatial.neighbors <- function(x, r, na.rm=FALSE) {
	coord <- coord(x)
	coordLabels <- names(coord)
	if ( r == 0 )
		return(matrix(seq_len(nrow(coord)), ncol=nrow(coord)))
	coord <- data.frame(lapply(coord, as.integer))
	dim <- sapply(coord, max)
	positionArray <- generatePositionArray(coord + r, dim + (2 * r))
	coord <- coord + r
	f <- function(...) positionArray[...]
	radii <- rep(list((-r):r), ncol(coord))
	names(radii) <- names(coord)
	radii[!names(coord) %in% coordLabels] <- 0
	lapply(seq_len(nrow(coord)), function(i) {
		neighbors <- do.call(f, mapply(`+`, coord[i,], radii, SIMPLIFY=FALSE))
		if ( na.rm ) neighbors[is.na(neighbors)] <- i
		dimnames(neighbors) <- radii[coordLabels]
		neighbors
	})
}

