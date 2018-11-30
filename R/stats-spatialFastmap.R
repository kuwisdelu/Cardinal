


.spatialFastmap <- function(x, r, ncomp, method, ...) {
	distfun <- .spatialDistanceFun(x, r, method)
	fastmap2(x, distfun=distfun, ncomp=ncomp, ...)
}

.spatialDistanceFun <- function(x, r, method) {
	bilateral <- switch(method,
		gaussian=FALSE, adaptive=TRUE)
	sigma <- ((2 * mean(r)) + 1) / 4
	neighbors <- findNeighbors(x, r=r)
	coord <- as.matrix(coord(x)[,coordLabels(x),drop=FALSE])
	offsets <- lapply(1:ncol(x), function(i) {
		.findSpatialOffsets(coord, neighbors, i)
	})
	weights <- mapply(function(ii, pos) {
		xi <- iData(x)[,ii]
		.findSpatialWeights(xi, pos, sigma, bilateral=bilateral)
	}, neighbors, offsets, SIMPLIFY=FALSE)
	X <- iData(x)
	function(x, i, j) {
		xi <- X[,neighbors[[i]],drop=FALSE]
		xj <- X[,neighbors[[j]],drop=FALSE]
		.findSpatialDistance(xi, xj, offsets[[i]], offsets[[j]],
			weights[[i]], weights[[j]], sigma)
	}
}
