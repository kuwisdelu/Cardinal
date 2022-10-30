
#### Spatially-aware methods ####

## Find neighborhood members

setMethod("findNeighbors", "ImagingExperiment",
	function(x, r = 1, groups = run(x), ...)
	{
		findNeighbors(pixelData(x), r=r, groups=groups, ...)
	})

setMethod("findNeighbors", "PositionDataFrame",
	function(x, r = 1, groups = run(x), dist = "chebyshev",
		offsets = FALSE, matrix = FALSE, ...)
	{
		if ( length(r) > 1L ) {
			.warning("r has length > 1; using r = ", r[1])
			r <- r[1]
		}
		coord <- as.matrix(coord(x))
		nb <- .findNeighbors(coord, r=r, groups=groups, dist=dist, matrix=matrix)
		if ( offsets )
			attr(nb, "offsets") <- .spatialOffsets(coord, nb)
		nb
	})

.findNeighbors <- function(coord, r, groups, dist, matrix = FALSE) {
	if ( !is.matrix(coord) )
		coord <- as.matrix(coord)
	groups <- rep_len(groups, nrow(coord))
	dist.types <- c("radial", "manhattan", "minkowski", "chebyshev")
	dist <- factor(match.arg(dist, dist.types), levels=dist.types)
	nb <- .Call("C_findNeighbors", coord, as.numeric(r),
		as.integer(groups), as.integer(dist), PACKAGE="Cardinal")
	if ( matrix ) {
		ones <- lapply(nb, function(i) rep_len(1L, length(i)))
		nb <- sparse_mat(data=list(keys=nb, values=ones),
			nrow=length(nb), ncol=length(nb), rowMaj=TRUE)
	}
	attr(nb, "r") <- r
	nb
}

## Calculate spatial weights

setMethod("spatialWeights", "ImagingExperiment",
	function(x, r = 1, method = c("gaussian", "adaptive"),
		dist = "chebyshev", matrix = FALSE, BPPARAM = getCardinalBPPARAM(), ...)
	{
		method <- match.arg(method)
		bilateral <- switch(method, gaussian=FALSE, adaptive=TRUE)
		if ( length(r) > 1L ) {
			.warning("r has length > 1; using r = ", r[1])
			r <- r[1]
		}
		sigma <- ((2 * r) + 1) / 4
		nb <- findNeighbors(x, r=r, dist=dist, offsets=TRUE)
		offsets <- attr(nb, "offsets")
		fun <- function(xi) {
			wts <- vector("list", attr(xi, "chunksize"))
			ii <- 1
			for ( i in seq_len(ncol(xi)) ) {
				di <- attr(xi, "index")[i]
				dp <- attr(xi, "depends")[[i]]
				if ( is.null(dp) )
					next
				wts[[ii]] <- .spatialWeights(xi[,dp,drop=FALSE],
					offsets=offsets[[di]], sigma=sigma,
					bilateral=bilateral)
				ii <- ii + 1
			}
			wts
		}
		weights <- chunk_colapply(iData(x), FUN=fun, depends=nb,
			nchunks=getCardinalNumBlocks(),
			verbose=FALSE, BPPARAM=BPPARAM)
		if ( matrix ) {
			w <- lapply(weights, function(w) w$alpha * w$beta)
			weights <- sparse_mat(data=w, index=c(nb),
				nrow=length(nb), ncol=length(nb), rowMaj=TRUE)
		} else {
			attr(weights, "offsets") <- offsets
			attr(weights, "neighbors") <- c(nb) # remove attr
		}
		weights
	})

setMethod("spatialWeights", "PositionDataFrame",
	function(x, r = 1, matrix = FALSE, ...)
	{
		.gaussianWeights(x, r=r, matrix=matrix, ...)
	})

.gaussianWeights <- function(x, r, matrix = FALSE, ...) {
	nb <- findNeighbors(x, r=r, offsets=TRUE, ...)
	offsets <- attr(nb, "offsets")
	sigma <- ((2 * r[1]) + 1) / 4
	weights <- mapply(function(pos) {
		.spatialWeights(NULL, offsets=pos,
			sigma=sigma, bilateral=FALSE)
	}, offsets, SIMPLIFY=FALSE)
	if ( matrix ) {
		attr(nb, "offsets") <- NULL
		w <- lapply(weights, function(w) w$alpha * w$beta)
		weights <- sparse_mat(data=w, index=c(nb),
			nrow=length(nb), ncol=length(nb), rowMaj=TRUE)
	} else {
		attr(weights, "offsets") <- offsets
		attr(weights, "neighbors") <- c(nb) # remove attr
	}
	weights
}

# spatial offsets for a list of neighborhoods
.spatialOffsets <- function(coord, neighbors) {
	if ( !is.matrix(coord) )
		coord <- as.matrix(coord)
	if ( is.sparse(neighbors) )
		neighbors <- atomdata(neighbors)$keys
	lapply(1:nrow(coord), function(i) {
		offsets <- .Call("C_spatialOffsets", coord,
			neighbors[[i]] - 1L, i - 1L, PACKAGE="Cardinal")
		colnames(offsets) <- colnames(coord)
		offsets
	})
}

# spatial weights for a single neighborhood
.spatialWeights <- function(x, offsets, sigma, bilateral = TRUE) {
	if ( is.null(x) ) {
		x <- numeric()
		bilaterial <- FALSE
	}
	if ( missing(sigma) )
		sigma <- ((2 * max(abs(offsets))) + 1) / 4
	weights <- .Call("C_spatialWeights", x, offsets, sigma, bilateral, PACKAGE="Cardinal")
	names(weights) <- c("alpha", "beta")
	weights
}

# spatial distances for a spatial block
.spatialDistance <- function(x, ref, offsets, ref.offsets,
	weights, ref.weights, neighbors, metric, tol.dist = 1e-9)
{
	if ( typeof(x) != typeof(ref) )
		.stop("arrays must have the same data type")
	if ( metric %in% c("average", "correlation") ) {
		x2 <- .spatialFilter(x, weights=weights,
			neighbors=neighbors)
		ref2 <- .spatialFilter(ref, weights=list(ref.weights),
			neighbors=list(seq_len(ncol(ref))))
		if ( metric == "average" ) {
			d <- sqrt(colSums((x2 - as.numeric(ref2))^2))
		} else if ( metric == "correlation") {
			d <- as.numeric(sqrt(2 * (1 - cor(x2, ref2))))
		} else {
			.stop("unrecognized dissimilarity metric")
		}
	} else if ( metric == "neighborhood" ) {
		d <- .Call("C_spatialDistance", x, ref, offsets, ref.offsets,
			weights, ref.weights, neighbors, tol.dist, PACKAGE="Cardinal")
	} else {
		.stop("unrecognized dissimilarity metric")
	}
	pmax(d, 0)
}

# spatial filter the column-vectors of a matrix
.spatialFilter <- function(x, weights, neighbors)
{
	if ( missing(neighbors) )
		neighbors <- attr(weights, "neighbors")
	if ( length(weights) != length(neighbors) )
		.stop("length of weights and neighbors must match")
	.Call("C_spatialFilter", x, weights, neighbors, PACKAGE="Cardinal")
}

# spatial discriminant scores for a spatial block
.spatialScores <- function(x, centers, weights, neighbors, sd)
{
	if ( missing(neighbors) )
		neighbors <- attr(weights, "neighbors")
	if ( nrow(x) != nrow(centers) || nrow(x) != length(sd) )
		.stop("number of features must match between arrays")
	if ( length(weights) != length(neighbors) )
		.stop("length of weights and neighbors must match")
	.Call("C_spatialScores", x, centers, weights, neighbors, sd, PACKAGE="Cardinal")
}


