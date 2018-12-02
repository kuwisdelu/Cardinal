
setAs("SpatialFastmap", "SpatialFastmap2",
	function(from) {
		.coerce_ResultImagingExperiment(from, "SpatialFastmap2")
	})

.spatialFastmap2 <- function(x, r, ncomp, method, ...) {
	init <- TRUE
	bilateral <- switch(method, gaussian=FALSE, adaptive=TRUE)
	spatial <- .spatialInfo(x, r=r, bilateral=bilateral)
	proj <- matrix(0, nrow=ncol(x), ncol=ncomp)
	pivots <- matrix(NA_integer_, nrow=ncomp, ncol=2)
	progress <- getOption("Cardinal.progress")
	options(Cardinal.progress=FALSE)
	for ( j in seq_len(ncomp) ) {
		.message("projecting FastMap component ", j, " ...")
		o_ab <- .findDistantObjects2(x, proj, spatial, init, ...)
		if ( any(is.na(o_ab)) )
			break		
		if ( !is.null(attr(o_ab, "init")) )
			init <- attr(o_ab, "init")
		pivots[j,] <- o_ab
		oa <- pivots[j,1]
		ob <- pivots[j,2]
		xa <- iData(x)[,spatial$neighbors[[oa]]]
		attr(xa, "idx") <- oa
		attr(xa, "offsets") <- spatial$offsets[[oa]]
		attr(xa, "weights") <- spatial$weights[[oa]]
		xb <- iData(x)[,spatial$neighbors[[ob]]]
		attr(xb, "idx") <- ob
		attr(xb, "offsets") <- spatial$offsets[[ob]]
		attr(xb, "weights") <- spatial$weights[[ob]]
		d_ab <- .spatialDistance2(xa, xb,
			spatial$offsets[[oa]], spatial$offsets[[ob]],
			spatial$weights[[oa]], spatial$weights[[ob]],
			proj=proj, i=oa, j=ob)
		fun <- function(xbl) {
			idx <- attr(xbl, "idx")
			ci <- seq_along(attr(xbl, "centers"))
			vapply(ci, function(k) {
				i <- match(attr(xbl, "neighbors")[[k]], idx)
				d_ai <- .spatialDistance2(xbl[,i,drop=FALSE], xa,
					attr(xbl, "offsets")[[k]], attr(xa, "offsets"),
					attr(xbl, "params")[[k]], attr(xa, "weights"),
					proj=proj, i=attr(xbl, "centers")[[k]], j=attr(xa, "idx"))
				d_bi <- .spatialDistance2(xbl[,i,drop=FALSE], xb,
					attr(xbl, "offsets")[[k]], attr(xb, "offsets"),
					attr(xbl, "params")[[k]], attr(xb, "weights"),
					proj=proj, i=attr(xbl, "centers")[[k]], j=attr(xb, "idx"))
				(d_ai^2 + d_ab^2 - d_bi^2) / (2 * d_ab)
			}, numeric(1))
		}
		comp_j <- spatialApply(x, .r=spatial$r, .fun=fun,
			.blocks=TRUE, .simplify=.unlist_and_reorder,
			.init=init, .params=spatial$weights)
		proj[,j] <- comp_j
	}
	.message("done.")
	options(Cardinal.progress=progress)
	list(scores=proj, pivot.array=pivots)
}

.findDistantObjects2 <- function(x, proj, spatial, init, iter.max = 3) {
	iter <- 1
	oa <- 1
	ob <- NULL
	fun <- function(xbl, xj) {
		idx <- attr(xbl, "idx")
		ci <- seq_along(attr(xbl, "centers"))
		vapply(ci, function(k) {
			i <- match(attr(xbl, "neighbors")[[k]], idx)
			.spatialDistance2(xbl[,i,drop=FALSE], xj,
				attr(xbl, "offsets")[[k]], attr(xj, "offsets"),
				attr(xbl, "params")[[k]], attr(xj, "weights"),
				proj=proj, i=attr(xbl, "centers")[[k]], j=attr(xj, "idx"))
		}, numeric(1))
	}
	while ( iter <= iter.max ) {
		xa <- iData(x)[,spatial$neighbors[[oa]]]
		attr(xa, "idx") <- oa
		attr(xa, "offsets") <- spatial$offsets[[oa]]
		attr(xa, "weights") <- spatial$weights[[oa]]
		dists <- spatialApply(x, .r=spatial$r, .fun=fun, xj=xa,
			.blocks=TRUE, .simplify=.unlist_and_reorder,
			.init=init, .params=spatial$weights)
		if ( !is.null(attr(dists, "init")) )
			init <- attr(dists, "init")
		cand <- which.max(dists)
		if ( dists[cand] == 0 )
			return(structure(c(NA, NA), init=init))
		if ( isTRUE(ob == cand) )
			return(structure(c(oa, ob), init=init))
		ob <- cand
		xb <- iData(x)[,spatial$neighbors[[ob]]]
		attr(xb, "idx") <- ob
		attr(xb, "offsets") <- spatial$offsets[[ob]]
		attr(xb, "weights") <- spatial$weights[[ob]]
		dists <- spatialApply(x, .r=spatial$r, .fun=fun, xj=xb,
			.blocks=TRUE, .simplify=.unlist_and_reorder,
			.init=init, .params=spatial$weights)
		oa <- which.max(dists)
		if ( dists[oa] == 0 )
			return(structure(c(NA, NA), init=init))
		iter <- iter + 1
	}
	structure(c(oa, ob), init=init)
}

.unlist_and_reorder <- function(ans) {
	unlist(ans)[order(unlist(attr(ans, "idx")))]
}

.spatialDistance2 <- function(x, y, xoffsets, yoffsets,
	xweights, yweights, proj, i, j, tol.dist = 1e-9)
{
	d <- .spatialDistance(x, y, xoffsets, yoffsets,
		xweights, yweights, tol.dist=tol.dist)
	d2 <- d^2 - sum((proj[i,] - proj[j,])^2)
	if ( d2 > 0 ) {
		sqrt(d2)
	} else {
		0
	}
}
