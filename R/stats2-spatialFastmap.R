
setMethod("spatialFastmap", "SparseImagingExperiment",
	function(x, r = 1, ncomp = 2,
		method = c("gaussian", "adaptive"),
		iter.max = 1,
		BPPARAM = bpparam(), ...)
	{
		method <- match.arg(method)
		if ( !is.list(BPPARAM) )
			BPPARAM <- list(BPPARAM, SerialParam())
		if ( max(ncomp) > nrow(x) )
			.stop("can't fit more components than number of pixels")
		if ( max(ncomp) > nrow(x) )
			.stop("can't fit more components than number of features")
		if ( length(ncomp) > 1L ) {
			ncomp <- max(ncomp)
			.warning("ncomp has length > 1; using ncomp = ", ncomp)
		}
		.message("fitting ", ncomp, " FastMap components with ",
			method, " spatial weights")
		results <- bplapply(r, function(ri, BPPARAM) {
			.message("r = ", ri)
			.spatialFastmap2(x=x, r=ri, ncomp=ncomp,
				method=method, iter.max=iter.max,
				BPPARAM=BPPARAM, ...)
		}, BPPARAM=BPPARAM)
		models <- DataFrame(r=r, ncomp=ncomp)
		.SpatialFastmap2(
			imageData=.SimpleImageArrayList(),
			featureData=featureData(x),
			elementMetadata=pixelData(x),
			metadata=list(resultType=list(
				feature="correlations",
				pixel="scores")),
			resultData=as(results, "List"),
			modelData=models)
	})

setAs("SpatialFastmap", "SpatialFastmap2",
	function(from) {
		to <- .coerce_ResultImagingExperiment(from, "SpatialFastmap2")
		metadata(to)$resultType <- list(feature="correlations", pixel="scores")
		to
	})

.spatialFastmap2 <- function(x, r, ncomp, method, iter.max = 2, ...) {
	init <- TRUE
	bilateral <- switch(method, gaussian=FALSE, adaptive=TRUE)
	spatial <- .spatialInfo(x, r=r, bilateral=bilateral)
	proj <- matrix(0, nrow=ncol(x), ncol=ncomp)
	pivots <- matrix(NA_integer_, nrow=ncomp, ncol=2)
	progress <- getOption("Cardinal.progress")
	options(Cardinal.progress=FALSE)
	fun <- function(xbl, xa, xb) {
		idx <- attr(xbl, "idx")
		f <- function(i, neighbors, offsets, weights) {
			d_ai <- .spatialDistance2(xbl[,neighbors,drop=FALSE], xa,
				offsets, attr(xa, "offsets"),
				weights, attr(xa, "weights"),
				proj=proj, i=idx[i], j=attr(xa, "idx"))
			d_bi <- .spatialDistance2(xbl[,neighbors,drop=FALSE], xb,
				offsets, attr(xb, "offsets"),
				weights, attr(xb, "weights"),
				proj=proj, i=idx[i], j=attr(xb, "idx"))
			(d_ai^2 + d_ab^2 - d_bi^2) / (2 * d_ab)
		}
		mapply(f, attr(xbl, "centers"), attr(xbl, "neighbors"),
			attr(xbl, "offsets"), attr(xbl, "params"))
	}
	for ( j in seq_len(ncomp) ) {
		.message("projecting component ", j)
		o_ab <- .findDistantObjects2(x, proj, spatial, init, iter.max, ...)
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
		comp_j <- spatialApply(x, .r=spatial$r, .fun=fun, xa=xa, xb=xb,
			.blocks=TRUE, .simplify=.unlist_and_reorder,
			.init=init, .params=spatial$weights, ...)
		proj[,j] <- comp_j
	}
	do_rbind <- function(ans) do.call("rbind", ans)
	corr <- featureApply(x, function(xbl) {
		t(apply(xbl, 1, function(xi) {
			vapply(1:ncomp, function(j) {
				comp_j <- proj[,j]
				if ( all(comp_j == 0) ) {
					0
				} else {
					cor(xi, comp_j)
				}
			}, numeric(1))
		}))
	}, .blocks=TRUE, .simplify=do_rbind, ...)
	colnames(proj) <- paste("FC", 1:ncomp, sep="")
	colnames(corr) <- paste("FC", 1:ncomp, sep="")
	pivots <- as.data.frame(pivots)
	names(pivots) <- c("Oa", "Ob")
	sdev <- apply(proj, 2, sd)
	options(Cardinal.progress=progress)
	SimpleList(scores=proj, correlations=corr, pivots=pivots, sdev=sdev)
}

.findDistantObjects2 <- function(x, proj, spatial, init, iter.max, ...) {
	iter <- 1
	oa <- 1
	ob <- NULL
	fun <- function(xbl, xj) {
		idx <- attr(xbl, "idx")
		f <- function(i, neighbors, offsets, weights) {
			.spatialDistance2(xbl[,neighbors,drop=FALSE], xj,
				offsets, attr(xj, "offsets"),
				weights, attr(xj, "weights"),
				proj=proj, i=idx[i], j=attr(xj, "idx"))
		}
		mapply(f, attr(xbl, "centers"), attr(xbl, "neighbors"),
			attr(xbl, "offsets"), attr(xbl, "params"))
	}
	while ( iter <= iter.max ) {
		xa <- iData(x)[,spatial$neighbors[[oa]]]
		attr(xa, "idx") <- oa
		attr(xa, "offsets") <- spatial$offsets[[oa]]
		attr(xa, "weights") <- spatial$weights[[oa]]
		dists <- spatialApply(x, .r=spatial$r, .fun=fun, xj=xa,
			.blocks=TRUE, .simplify=.unlist_and_reorder,
			.init=init, .params=spatial$weights, ...)
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
			.init=init, .params=spatial$weights, ...)
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
