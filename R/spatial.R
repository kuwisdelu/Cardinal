
#### Spatially-aware statistics ####

## Calculate alpha, beta, and neighborhood information
## 'x' is an SImageSet object
## 'r' is the neighborhood radius
## 'method' spatially-aware (SA) of spatially-aware structurally-adaptive (SASA)
spatial.info <- function(x, r, method) {
	neighbors <- spatial.neighbors(x, r=r, na.rm=TRUE)
	alpha <- spatial.alpha(r=r, p=length(coordLabels(x)))
	if ( method == "adaptive" ) {
		beta <- spatial.beta(x, neighbors)
	} else {
		beta <- rep(list(matrix(1, nrow=nrow(alpha), ncol=nrow(alpha))), ncol(iData(x)))
	}
	list(neighbors=neighbors, alpha=alpha, beta=beta)
}

## Gaussian spatial weights
## 'r' is the neighborhood radius
## 'p' is the number of dimenions (2D or 3D?)
spatial.alpha <- function(r, p) {
	sigma <- ((2 * r) + 1) / 4
	radii <- lapply(1:p, function(i) (-r):r)
	neighborhood <- expand.grid(radii)
	alpha <- apply(neighborhood, 1, function(i) exp((-sum(i^2))/(2 * sigma^2)))
	dim(alpha) <- rep((2 * r) + 1, p)
	alpha
}

## Adaptive spatial weights
## 'x' is an SImageSet object
## 'neighbors' is a list of neighbor indices
spatial.beta <- function(x, neighbors) {
	if ( length(neighbors[[1]]) == 1 ) {
		matrix(1, ncol=length(neighbors))
	} else {
		mapply(function(i, nb) {
			delta <- sqrt(colSums((iData(x)[,i] - iData(x)[,nb,drop=FALSE])^2))
			lambda <- max(delta - min(delta)) / 2
			lambda <- ifelse(lambda > 0, lambda, 1)
			beta <- exp(-delta^2 / (2 * lambda^2))
			dim(beta) <- dim(nb)
			dimnames(beta) <- dimnames(nb)
			beta
		}, seq_len(ncol(iData(x))), neighbors, SIMPLIFY=FALSE)
	}
}

## Find neighboring pixels within a given radius
## 'x' is an SImageSet object
## 'r' is the neighborhood radius
## 'indices' is the pixels for which to find the neighborhood
## 'na.rm' replaces out-of-bound indices with a pixel's own index
spatial.neighbors <- function(x, r, indices, na.rm=FALSE) {
	coord <- coord(x)
	if ( length(r) > 1 ) {
		warning("'r' must be length 1, only the first element will be used")
		r <- r[1]
	}
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
	if ( missing(indices) )
		indices <- seq_len(nrow(coord))
	lapply(indices, function(i) {
		neighbors <- do.call(f, mapply(`+`, coord[i,], radii, SIMPLIFY=FALSE))
		if ( na.rm ) neighbors[is.na(neighbors)] <- i
		dimnames(neighbors) <- radii[coordLabels(x)]
		neighbors
	})
}

