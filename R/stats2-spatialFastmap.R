
setMethod("spatialFastmap", "SparseImagingExperiment",
	function(x, r = 1, ncomp = 3,
		method = c("gaussian", "adaptive"),
		metric = c("average", "correlation", "neighborhood"),
		dist = "chebyshev", tol.dist = 1e-9,
		iter.max = 1, BPPARAM = getCardinalBPPARAM(), ...)
	{
		.checkForIncompleteProcessing(x)
		BPPARAM <- .protectNestedBPPARAM(BPPARAM)
		method <- match.arg(method)
		metric <- match.arg(metric)
		if ( max(ncomp) > ncol(x) )
			.stop("can't fit more components than number of pixels")
		if ( max(ncomp) > nrow(x) )
			.stop("can't fit more components than number of features")
		if ( length(ncomp) > 1L )
			ncomp <- max(ncomp)
		.message("projecting ", ncomp, " FastMap components")
		.message("using ", metric, " dissimilarity")
		.message("using ", method, " weights")
		results <- bplapply(r, function(ri, BPPARAM) {
			.message("r = ", ri, " ", appendLF=FALSE)
			.spatialFastmap2(x=x, r=ri, ncomp=ncomp, method=method,
				metric=metric, dist=dist, tol.dist=tol.dist,
				iter.max=iter.max, BPPARAM=BPPARAM)
		}, BPPARAM=BPPARAM)
		models <- DataFrame(r=r, ncomp=ncomp)
		.SpatialFastmap2(
			imageData=.SimpleImageArrayList(),
			featureData=featureData(x),
			elementMetadata=pixelData(x),
			metadata=list(
				mapping=list(
					feature="correlation",
					pixel="scores"),
				method=method, dist=dist,
				metric=metric),
			resultData=as(results, "List"),
			modelData=models)
	})

setAs("SpatialFastmap", "SpatialFastmap2",
	function(from) {
		to <- .coerce_ImagingResult(from, "SpatialFastmap2")
		metadata(to)$mapping <- list(feature="correlation", pixel="scores")
		to
	})

.spatialFastmap2 <- function(x, r, ncomp, method, metric, dist,
							tol.dist, iter.max = 2, BPPARAM)
{
	wts <- spatialWeights(x, r=r, method=method,
						dist=dist, BPPARAM=BPPARAM)
	spatial <- list(r=r, weights=c(wts),
		neighbors=attr(wts, "neighbors"),
		offsets=attr(wts, "offsets"))
	proj <- matrix(0, nrow=ncol(x), ncol=ncomp)
	pivots <- matrix(NA_integer_, nrow=ncomp, ncol=2)
	# spatially-aware distance functions
	fun <- function(xbl, xa, xb) {
		d_ai <- .spatialDistance2(xbl, xa,
			offsets=attr(xbl, "offsets"),
			weights=attr(xbl, "weights"),
			ref.offsets=attr(xa, "offsets"),
			ref.weights=attr(xa, "weights"),
			neighbors=attr(xbl, "neighbors"),
			metric=metric,
			i=attr(xbl, "idx"),
			j=attr(xa, "idx"),
			proj=proj,
			tol.dist=tol.dist)
		d_bi <- .spatialDistance2(xbl, xb,
			offsets=attr(xbl, "offsets"),
			weights=attr(xbl, "weights"),
			ref.offsets=attr(xb, "offsets"),
			ref.weights=attr(xb, "weights"),
			neighbors=attr(xbl, "neighbors"),
			metric=metric,
			i=attr(xbl, "idx"),
			j=attr(xb, "idx"),
			proj=proj,
			tol.dist=tol.dist)
		(d_ai^2 + d_ab^2 - d_bi^2) / (2 * d_ab)
	}
	# iterate over FastMap components
	for ( j in seq_len(ncomp) ) {
		o_ab <- .findDistantObjects2(x, proj=proj, spatial=spatial,
			metric=metric, dist=dist, tol.dist=tol.dist,
			iter.max=iter.max, BPPARAM=BPPARAM)
		if ( any(is.na(o_ab)) )
			break		
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
			offsets=list(spatial$offsets[[oa]]),
			weights=list(spatial$weights[[oa]]),
			ref.offsets=spatial$offsets[[ob]],
			ref.weights=spatial$weights[[ob]],
			neighbors=list(seq_len(ncol(xa))),
			metric=metric, i=oa, j=ob,
			proj=proj, tol.dist=tol.dist)
		comp_j <- spatialApply(x, .r=spatial$r, .fun=fun, xa=xa, xb=xb,
			.simplify=.unlist_once, .verbose=FALSE, .dist=dist,
			.params=list(weights=spatial$weights),
			.view="chunk", BPPARAM=BPPARAM)
		.message(".", appendLF=FALSE)
		proj[,j] <- comp_j
	}
	.message(" ")
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
	}, .simplify=do_rbind, .verbose=FALSE, .view="chunk", BPPARAM=BPPARAM)
	colnames(proj) <- paste("FC", 1:ncomp, sep="")
	colnames(corr) <- paste("FC", 1:ncomp, sep="")
	pivots <- as.data.frame(pivots)
	names(pivots) <- c("Oa", "Ob")
	sdev <- apply(proj, 2, sd)
	list(scores=proj, correlation=corr, pivots=pivots, sdev=sdev)
}

.findDistantObjects2 <- function(x, proj, spatial, metric, dist,
							tol.dist, iter.max, BPPARAM)
{
	iter <- 1
	oa <- 1
	ob <- NULL
	fun <- function(xbl, xj) {
		.spatialDistance2(xbl, xj,
			offsets=attr(xbl, "offsets"),
			weights=attr(xbl, "weights"),
			ref.offsets=attr(xj, "offsets"),
			ref.weights=attr(xj, "weights"),
			neighbors=attr(xbl, "neighbors"),
			metric=metric,
			i=attr(xbl, "idx"),
			j=attr(xj, "idx"),
			proj=proj,
			tol.dist=tol.dist)
	}
	while ( iter <= iter.max ) {
		xa <- iData(x)[,spatial$neighbors[[oa]]]
		attr(xa, "idx") <- oa
		attr(xa, "offsets") <- spatial$offsets[[oa]]
		attr(xa, "weights") <- spatial$weights[[oa]]
		dists <- spatialApply(x, .r=spatial$r, .fun=fun, xj=xa,
			.simplify=.unlist_once, .verbose=FALSE, .dist=dist,
			.params=list(weights=spatial$weights),
			.view="chunk", BPPARAM=BPPARAM)
		cand <- which.max(dists)
		if ( dists[cand] == 0 )
			return(c(NA, NA))
		if ( isTRUE(ob == cand) )
			return(c(oa, ob))
		ob <- cand
		xb <- iData(x)[,spatial$neighbors[[ob]]]
		attr(xb, "idx") <- ob
		attr(xb, "offsets") <- spatial$offsets[[ob]]
		attr(xb, "weights") <- spatial$weights[[ob]]
		dists <- spatialApply(x, .r=spatial$r, .fun=fun, xj=xb,
			.simplify=.unlist_once, .verbose=FALSE, .dist=dist,
			.params=list(weights=spatial$weights),
			.view="chunk", BPPARAM=BPPARAM)
		oa <- which.max(dists)
		d <- dists[oa]
		if ( dists[oa] == 0 )
			return(c(NA, NA))
		iter <- iter + 1
	}
	c(oa, ob)
}


.spatialDistance2 <- function(x, ref, offsets, ref.offsets,
	weights, ref.weights, neighbors, metric, i, j, proj, tol.dist)
{
	d <- .spatialDistance(x, ref, offsets, ref.offsets,
		weights, ref.weights, neighbors, metric, tol.dist)
	proj_i <- proj[i,,drop=FALSE]
	proj_j <- proj[rep_len(j, length(i)),,drop=FALSE]
	d2 <- d^2 - rowSums((proj_i - proj_j)^2)
	sqrt(replace(d2, d2 < 0, 0))
}

