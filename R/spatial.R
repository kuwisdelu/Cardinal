
#### Spatially-aware statistics ####

## Calculate neighborhood members and spatial weights

setMethod("findNeighbors", "PositionDataFrame",
	function(x, r, groups = run(x), ...)
	{
		if ( length(r) != length(coord(x)) ) {
			r <- rep_len(unname(r), length(coord(x)))
		} else if ( !is.null(names(r)) ) {
			r <- r[names(coord(x))]
		}
		coord <- as.matrix(coord(x))
		out <- .Call("findNeighbors", coord, as.numeric(r),
			as.integer(groups), PACKAGE="Cardinal")
		attr(out, "r") <- r
		out
	})

setMethod("findNeighbors", "ImagingExperiment",
	function(x, r, groups = run(x), ...) {
		findNeighbors(pixelData(x), r=r, groups=groups, ...)
	})

setMethod("findNeighbors", "iSet",
	function(x, r, groups = pData(x)$sample, ...)
	{
		if ( length(r) != length(coordLabels(x)) ) {
			r <- rep_len(unname(r), length(coordLabels(x)))
		} else if ( !is.null(names(r)) ) {
			r <- r[coordLabels(x)]
		}
		coord <- as.matrix(coord(x)[,coordLabels(x),drop=FALSE])
		out <- .Call("findNeighbors", coord, as.numeric(r), as.integer(groups), PACKAGE="Cardinal")
		attr(out, "r") <- r
		out
	})

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
	blocks <- .Call("findSpatialBlocks", as.matrix(coord(x)),
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

.whichSpatialBlocks <- function(neighbors, blocks) {
	.Call("whichSpatialBlocks", neighbors, blocks, PACKAGE="Cardinal")
}

.spatialOffsets <- function(coord, neighbors, i) {
	if ( !is.matrix(coord) )	
		coord <- as.matrix(coord)
	if ( is.list(neighbors) )
		neighbors <- neighbors[[i]]
	offsets <- .Call("spatialOffsets", coord, neighbors - 1L, i - 1L, PACKAGE="Cardinal")
	colnames(offsets) <- colnames(coord)
	offsets
}

.spatialWeights <- function(x, offsets, sigma, bilateral = FALSE) {
	if ( missing(sigma) )
		sigma <- ((2 * max(abs(offsets))) + 1) / 4
	weights <- .Call("spatialWeights", x, offsets, sigma, bilateral, PACKAGE="Cardinal")
	names(weights) <- c("alpha", "beta")
	weights
}

.spatialInfo <- function(x, r, weights = TRUE, bilateral = FALSE) {
	neighbors <- findNeighbors(x, r=r)
	if ( is(x, "ImagingExperiment") ) {
		coord <- as.matrix(coord(x))
	} else if ( is(x, "iSet") ) {
		coord <- as.matrix(coord(x)[,coordLabels(x),drop=FALSE])
	} else {
		stop("unrecognized imaging class")
	}
	offsets <- lapply(1:ncol(x), function(i) {
		.spatialOffsets(coord, neighbors, i)
	})
	if ( weights ) {
		sigma <- ((2 * mean(r)) + 1) / 4
		weights <- mapply(function(ii, pos) {
			.spatialWeights(iData(x)[,ii], pos, sigma, bilateral)
		}, neighbors, offsets, SIMPLIFY=FALSE)
		info <- list(neighbors=neighbors, offsets=offsets, weights=weights)
	} else {
		info <- list(neighbors=neighbors, offsets=offsets)
	}
}

.spatialDistance <- function(x, y, xoffsets, yoffsets,
	xweights, yweights, sigma, tol.dist = 1e-9, bilateral = FALSE)
{
	if ( typeof(x) != typeof(y) || typeof(xoffsets) != typeof(yoffsets) )
		stop("arrays must have the same data type")
	if ( missing(sigma) )
		sigma <- ((2 * max(abs(xoffsets))) + 1) / 4
	if ( missing(xweights) )
		xweights <- .spatialWeights(x, xoffsets, sigma, bilateral)
	if ( missing(yweights) )
		yweights <- .spatialWeights(y, yoffsets, sigma, bilateral)
	.Call("spatialDistance", x, y, xoffsets, yoffsets,
		xweights, yweights, tol.dist, PACKAGE="Cardinal")
}

