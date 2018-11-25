
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
		.Call("findNeighbors", as.matrix(coord(x)),
			as.numeric(r), as.integer(groups), PACKAGE="Cardinal")
	})

.findSpatialBlocks <- function(x, r, groups, nblocks) {
	groups <- as.factor(groups)
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
		}, coord(x), block_len, r)
		grid <- as.matrix(expand.grid(sapply(limits,
			function(lim) seq_len(length(lim) - 1L))))
		list(limits=limits, grid=grid)
	})
	blocks <- .Call("findSpatialBlocks", as.matrix(coord(x)),
		as.numeric(r), as.integer(groups), block_info, PACKAGE="Cardinal")
	if ( length(blocks) > 1L ) {
		do.call("c", blocks)
	} else {
		blocks[[1L]]
	}
}

.validSpatialBlocks <- function(neighbors, blocks) {
	ok <- sapply(neighbors, function(nb) {
		any(sapply(blocks, function(bl) all(nb %in% bl)))
	})
	all(ok)
}

.whichSpatialBlocks <- function(neighbors, blocks) {
	wh <- vapply(neighbors, function(nb) {
		ok <- vapply(blocks, function(bl) all(nb %in% bl), logical(1))
		if ( !any(ok) )
			stop("invalid blocks")
		which(ok)[1L]
	}, integer(1))
	lapply(seq_along(blocks), function(i) which(wh == i))
}


