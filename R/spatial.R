
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
			attr(nb, "offsets") <- .getSpatialOffsets(coord, nb)
		nb
	})

setMethod("findNeighbors", "iSet",
	function(x, r = 1, groups = x$sample, ...)
	{
		findNeighbors(pixelData(x), r=r, groups=groups, ...)
	})

setMethod("findNeighbors", "IAnnotatedDataFrame",
	function(x, r = 1, groups = x$sample, dist = "chebyshev",
		offsets = FALSE, matrix = FALSE, ...)
	{
		if ( length(r) > 1L ) {
			.warning("r has length > 1; using r = ", r[1])
			r <- r[1]
		}
		coord <- as.matrix(coord(x)[,coordLabels(x),drop=FALSE])
		nb <- .findNeighbors(coord, r=r, groups=groups, dist=dist, matrix=matrix)
		if ( offsets )
			attr(nb, "offsets") <- .getSpatialOffsets(coord, nb)
		nb
	})

.findNeighbors <- function(coord, r, groups, dist, matrix = FALSE) {
	if ( !is.matrix(coord) )
		coord <- as.matrix(coord)
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

.getSpatialOffsets <- function(coord, neighbors) {
	if ( !is.matrix(coord) )
		coord <- as.matrix(coord)
	if ( is.sparse(neighbors) )
		neighbors <- atomdata(neighbors)$keys
	lapply(1:nrow(coord), function(i)
		.spatialOffsets(coord, neighbors, i))
}

## Calculate spatial weights

setMethod("spatialWeights", "ImagingExperiment",
	function(x, r = 1, method = c("gaussian", "adaptive"),
		dist = "chebyshev", matrix = FALSE, BPPARAM = bpparam(), ...)
	{
		method <- match.arg(method)
		bilateral <- switch(method, gaussian=FALSE, adaptive=TRUE)
		if ( length(r) > 1L ) {
			.warning("r has length > 1; using r = ", r[1])
			r <- r[1]
		}
		sigma <- ((2 * r) + 1) / 4
		fun <- function(xbl) {
			neighbors <- attr(xbl, "neighbors")
			offsets <- attr(xbl, "offsets")
			mapply(function(ii, pos) {
				.spatialWeights(xbl[,ii,drop=FALSE],
					offsets=pos, sigma=sigma, bilateral=bilateral)
			}, neighbors, offsets, SIMPLIFY=FALSE)
		}
		progress <- getOption("Cardinal.progress")
		options(Cardinal.progress=FALSE)
		weights <- spatialApply(x, .r=r, .fun=fun, ..., .dist=dist,
			.blocks=TRUE, .simplify=.unlist_and_reorder,
			.init=TRUE, BPPARAM=BPPARAM)
		options(Cardinal.progress=progress)
		nb <- attr(weights, "init")$spatial$neighbors
		attr(weights, "init") <- NULL
		if ( matrix ) {
			w <- lapply(weights, function(w) w$alpha * w$beta)
			weights <- sparse_mat(data=list(keys=nb, values=w),
				nrow=length(nb), ncol=length(nb), rowMaj=TRUE)
		} else {
			attr(weights, "neighbors") <- nb
		}
		weights
	})

setMethod("spatialWeights", "PositionDataFrame",
	function(x, r = 1, matrix = FALSE, ...)
	{
		.getSpatialWeights(x, r=r, matrix=matrix, ...)
	})

setMethod("spatialWeights", "iSet",
	function(x, r = 1, method = c("gaussian", "adaptive"), matrix = FALSE, ...)
	{
		method <- match.arg(method)
		bilateral <- switch(method, gaussian=FALSE, adaptive=TRUE)
		if ( length(r) > 1L ) {
			.warning("r has length > 1; using r = ", r[1])
			r <- r[1]
		}
		sigma <- ((2 * r) + 1) / 4
		nb <- findNeighbors(x, r=r, offsets=TRUE, ...)
		offsets <- attr(nb, "offsets")
		weights <- mapply(function(ii, pos) {
			.spatialWeights(iData(x)[,ii], pos, sigma, bilateral)
		}, nb, offsets, SIMPLIFY=FALSE)
		if ( matrix ) {
			attr(nb, "offsets") <- NULL
			w <- lapply(weights, function(w) w$alpha * w$beta)
			weights <- sparse_mat(data=list(keys=nb, values=w),
				nrow=length(nb), ncol=length(nb), rowMaj=TRUE)
		} else {
			attr(weights, "neighbors") <- nb
		}
		weights
	})

setMethod("spatialWeights", "IAnnotatedDataFrame",
	function(x, r = 1, matrix = FALSE, ...)
	{
		.getSpatialWeights(x, r=r, matrix=matrix, ...)
	})

.getSpatialWeights <- function(x, r, matrix = FALSE, ...) {
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
		weights <- sparse_mat(data=list(keys=nb, values=w),
			nrow=length(nb), ncol=length(nb), rowMaj=TRUE)
	} else {
		attr(weights, "neighbors") <- nb
	}
	weights
}

# summarize spatial information (for old-style class methods)
.spatialInfo <- function(x, r, dist = "chebyshev",
	weights = TRUE, method = c("gaussian", "adaptive"), ...)
{
	if ( length(r) > 1L ) {
		.warning("r has length > 1; using r = ", r[1])
		r <- r[1]
	}
	neighbors <- findNeighbors(x, r=r, offsets=TRUE, dist=dist)
	offsets <- attr(neighbors, "offsets")
	attr(neighbors, "offsets") <- NULL
	if ( weights ) {
		progress <- getOption("Cardinal.progress")
		options(Cardinal.progress=FALSE)
		weights <- spatialWeights(x, r=r, method=method, ...)
		options(Cardinal.progress=progress)
		list(neighbors=neighbors, offsets=offsets, weights=weights, r=r)
	} else {
		list(neighbors=neighbors, offsets=offsets, r=r)
	}
}

# split data into spatially-aware blocks for processing
.findSpatialBlocks <- function(x, r, groups, nblocks, neighbors = NULL) {
	groups <- as.factor(groups)
	if ( missing(r) && !is.null(neighbors) )
		r <- attr(neighbors, "r")
	if ( length(r) != length(coord(x)) ) {
		r <- rep_len(unname(r), length(coord(x)))
	} else if ( !is.null(names(r)) ) {
		r <- r[names(coord(x))]
	}
	# assume all groups are equal size
	nblocks_per_group <- nblocks / nlevels(groups)
	# calculate blocks for each group
	block_info <- lapply(levels(groups), function(gi) {
		gcoord <- coord(x)[groups == gi,,drop=FALSE]
		dim_len <- sapply(gcoord, function(pos) diff(range(pos)))
		block_per_dim <- round(nblocks_per_group^(dim_len/sum(dim_len)))
		block_len <- dim_len / block_per_dim
		limits <- mapply(function(pos, len, ri) {
			lim <- seq(from=min(pos), to=max(pos), by=max(len, 2 * ri))
			lim <- as.numeric(lim)
			if ( lim[length(lim)] >  max(pos) - ri )
				lim[length(lim)] <- max(pos)
			lim
		}, gcoord, block_len, r, SIMPLIFY=FALSE)
		grid <- lapply(limits, function(lim) seq_len(length(lim) - 1L))
		grid <- as.matrix(expand.grid(grid))
		list(limits=limits, grid=grid)
	})
	blocks <- .Call("C_findSpatialBlocks", as.matrix(coord(x)),
		as.numeric(r), as.integer(groups), block_info, PACKAGE="Cardinal")
	if ( length(blocks) > 1L ) {
		blocks <- do.call("c", blocks)
	} else {
		blocks <- blocks[[1L]]
	}
	blocks <- blocks[lengths(blocks) != 0L]
	if ( !is.null(neighbors) ) {
		wh <- .whichSpatialBlocks(neighbors, blocks)
		if ( anyNA(wh) )
			stop("invalid blocks")
		centers <- lapply(seq_along(blocks), function(i) which(wh == i))
		blocks <- blocks[lengths(centers) != 0L]
		centers <- centers[lengths(centers) != 0L]
		for ( i in seq_along(blocks) )
			attr(blocks[[i]], "centers") <- centers[[i]]
	}
	blocks
}

# assign pixels to a processing block
.whichSpatialBlocks <- function(neighbors, blocks) {
	.Call("C_whichSpatialBlocks", neighbors, blocks, PACKAGE="Cardinal")
}

# spatial offsets of a single neighborhood
.spatialOffsets <- function(coord, neighbors, i) {
	if ( !is.matrix(coord) )	
		coord <- as.matrix(coord)
	if ( is.list(neighbors) )
		neighbors <- neighbors[[i]]
	offsets <- .Call("C_spatialOffsets", coord, neighbors - 1L, i - 1L, PACKAGE="Cardinal")
	colnames(offsets) <- colnames(coord)
	offsets
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

# spatial distance for a single neighborhood
.spatialDistance <- function(x, y, xoffsets, yoffsets,
	xweights, yweights, sigma, tol.dist = 1e-9, bilateral = TRUE)
{
	if ( typeof(x) != typeof(y) || typeof(xoffsets) != typeof(yoffsets) )
		stop("arrays must have the same data type")
	if ( missing(sigma) )
		sigma <- ((2 * max(abs(xoffsets))) + 1) / 4
	if ( missing(xweights) )
		xweights <- .spatialWeights(x, xoffsets, sigma, bilateral)
	if ( missing(yweights) )
		yweights <- .spatialWeights(y, yoffsets, sigma, bilateral)
	.Call("C_spatialDistance", x, y, xoffsets, yoffsets,
		xweights, yweights, tol.dist, PACKAGE="Cardinal")
}
