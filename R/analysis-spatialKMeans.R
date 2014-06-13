
setMethod("spatialKMeans",
	signature = c(x = "SImageSet"),
	function(x, r = 1, k = 2,
		method = c("gaussian", "adaptive"),
		weights = 1, iter.max = 100, nstart = 100,
		algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
			"MacQueen"), ...)
{
	method <- match.arg(method)
	save.seed()
	rs <- sort(r)
	ks <- sort(k)
	w <- rep(weights, length.out=nrow(iData(x)))
	.time.start()
	out <- unlist(lapply(rs, function(r){
		spatial <- spatial.info(x, r=r, method=method)
		.message("spatialKMeans: Calculating spatial information for r = ", r, ".")
		fastmap <- spatial.fastmap(x, r=r, spatial=spatial, w=w, ...)
		lapply(ks, function(k) {
			.message("spatialKMeans: Fitting r = ", r, ", k = ", k, ".")
			res <- append(.spatialKMeans(x, fastmap=fastmap, k=k,
				iter.max=iter.max, nstart=nstart, algorithm=algorithm, ...),
				list(r=r, k=k, method=method, weights=weights,
					fastmap=fastmap))
			class(res) <- "ResultData"
			res
		})
	}), recursive=FALSE)
	.message("spatialKMeans: Preparing results.")
	par <- AnnotatedDataFrame(data=data.frame(
			r=sapply(out, function(fit) fit$r),
			k=sapply(out, function(fit) fit$k)),
		varMetadata=data.frame(
			labelDescription=c(
				k="Number of clusters",
				r="Neighborhood radius")))
	featureNames(par) <- formatParam(pData(par))
	names(out) <- formatParam(pData(par))
	.message("spatialKMeans: Done.")
	.time.stop()
	new("SpatialKMeans",
		pixelData=x@pixelData,
		featureData=x@featureData,
		experimentData=x@experimentData,
		protocolData=x@protocolData,
		resultData=out,
		modelData=par)
})

.spatialKMeans <- function(x, fastmap, k, iter.max, nstart, algorithm, ...) {
	restore.seed()
	start.time <- proc.time()
	cluster <- kmeans(fastmap$scores, centers=k,
		iter.max=iter.max, nstart=nstart,
		algorithm=algorithm)$cluster
	centers <- sapply(sort(unique(cluster)), function(Ck) {
		rowMeans(iData(x)[,cluster==Ck,drop=FALSE])
	})
	cluster <- factor(cluster)
	names(cluster) <- pixelNames(x)
	rownames(centers) <- featureNames(x)
	colnames(centers) <- levels(x)
	list(cluster=cluster, centers=centers, time=proc.time() - start.time)
}

spatial.info <- function(x, r, method) {
	neighbors <- spatial.neighbors(x, r=r, na.rm=TRUE)
	alpha <- spatial.alpha(r=r, p=length(coordLabels(x)))
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
	if ( r == 0 )
		return(matrix(seq_len(nrow(coord)), ncol=nrow(coord)))
	coord <- data.frame(lapply(coord, as.integer))
	dim <- sapply(coord, max)
	positionArray <- generatePositionArray(coord + r, dim + (2 * r))
	coord <- coord + r
	f <- function(...) positionArray[...]
	radii <- rep(list((-r):r), ncol(coord))
	names(radii) <- names(coord)
	radii[!names(coord) %in% coordLabels(x)] <- 0
	lapply(seq_len(nrow(coord)), function(i) {
		neighbors <- do.call(f, mapply(`+`, coord[i,], radii, SIMPLIFY=FALSE))
		if ( na.rm ) neighbors[is.na(neighbors)] <- i
		dimnames(neighbors) <- radii[names(coord)]
		neighbors
	})
}

spatial.fastmap <- function(x, r=1, ncomp=20, scale=FALSE, spatial, w, ...) {
	x <- as.matrix(iData(x))
	if ( scale ) x <- t(apply(x, 1, scale))
	alpha <- spatial$alpha
	beta <- t(simplify2array(spatial$beta, higher=FALSE))
	neighbors <- t(simplify2array(spatial$neighbors, higher=FALSE))
	ncomp <- ifelse(sum(w > 0) > ncomp, ncomp, sum(w > 0))
	x.new <- matrix(0, nrow=ncol(x), ncol=ncomp)
	pivot.array <- matrix(0, nrow=ncomp, ncol=2)
	start.time <- proc.time()
	out.fastmap <- .C("fastmap_spatial", as.double(x),
		as.integer(nrow(x)), as.integer(ncol(x)),
		as.integer(neighbors - 1), as.integer(ncol(neighbors)),
		as.double(w), as.double(alpha), as.double(beta),
		as.double(x.new), as.integer(pivot.array), as.integer(ncomp))
	x.new <- matrix(out.fastmap[[9]], nrow=ncol(x), ncol=ncomp)
	x.new <- as.data.frame(x.new)
	names(x.new) <- paste("FC", 1:ncomp, sep="")
	pivot.array <- matrix(out.fastmap[[10]] + 1, nrow=ncomp, ncol=2)
	pivot.array <- as.data.frame(pivot.array)
	names(pivot.array) <- c("Oa", "Ob")
	list(scores=x.new, pivot.array=pivot.array,
		time=proc.time() - start.time)
}

