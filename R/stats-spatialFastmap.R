
#### Spatially-aware FastMap ####
## ------------------------------

setMethod("spatialFastmap", "ANY",
	function(x, coord, r = 1, ncomp = 3,
		weights = c("gaussian", "adaptive"),
		neighbors = findNeighbors(coord, r=r),
		transpose = TRUE, niter = 3L,
		verbose = getCardinalVerbose(), chunkopts = list(),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( "method" %in% ...names() ) {
		.Deprecated(old="method", new="weights")
		weights <- list(...)$method
	}
	if ( is.character(weights) ) {
		if ( verbose )
			message("calculating gaussian weights")
		wts <- spatialWeights(as.matrix(coord), neighbors=neighbors)
		if ( match.arg(weights) == "adaptive" )
		{
			if ( verbose )
				message("calculating adaptive weights")
			awts <- spatialWeights(x, neighbors=neighbors,
				weights="adaptive", byrow=!transpose,
				verbose=verbose, chunkopts=chunkopts,
				BPPARAM=BPPARAM, ...)
			wts <- Map("*", wts, awts)
		}
	} else {
		wts <- rep_len(weights, length(neighbors))
		weights <- "user-provided weights"
	}
	if ( transpose ) {
		distfun <- .spatialColDistFun
	} else {
		distfun <- .spatialRowDistFun
	}
	ncomp <- min(ncomp, dim(x))
	ans <- fastmap(x, k=ncomp, distfun=distfun,
		neighbors=neighbors, neighbor.weights=wts,
		transpose=transpose, niter=niter,
		verbose=verbose, chunkopts=chunkopts,
		BPPARAM=BPPARAM, ...)
	ans$weights <- weights
	ans$r <- r
	if ( verbose )
		message("returning FastMap projection")
	ans
})

setMethod("spatialFastmap", "SpectralImagingExperiment", 
	function(x, r = 1, ncomp = 3,
		weights = c("gaussian", "adaptive"),
		neighbors = findNeighbors(x, r=r), ...)
{
	if ( length(processingData(x)) > 0L )
		warning("pending processing steps will be ignored")
	ans <- spatialFastmap(spectra(x),
		coord=coord(x), r=r, ncomp=ncomp,
		neighbors=neighbors, weights=weights, transpose=TRUE, ...)
	as(SpatialResults(ans, x), "SpatialFastmap")
})

setMethod("predict", "SpatialFastmap",
	function(object, newdata,
		neighbors = findNeighbors(newdata, r=object$r),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( !is(newdata, "SpectralImagingExperiment") )
		stop("'newdata' must inherit from 'SpectralImagingExperiment'")
	if ( length(processingData(newdata)) > 0L )
		warning("pending processing steps will be ignored")
	if ( nrow(newdata) != nrow(object$pivot.array) )
		stop("'newdata' does not have the correct number of dimensions")
	wts <- spatialWeights(as.matrix(coord(newdata)), neighbors=neighbors)
	if ( object$weights == "adaptive" )
	{
		awts <- spatialWeights(newdata, neighbors=neighbors,
			verbose=FALSE, BPPARAM=BPPARAM, ...)
		wts <- Map("*", wts, awts)
	}
	predict(object@model, newdata=spectra(newdata),
		neighbors=neighbors, neighbor.weights=wts,
		verbose=FALSE, BPPARAM=BPPARAM, ...)
})

setMethod("plot", c(x = "SpatialFastmap", y = "missing"),
	function(x, type = c("scree", "x"), ..., xlab, ylab)
{
	type <- match.arg(type)
	if ( type == "x" ) {
		callNextMethod(x, y=x$x, xlab=xlab, ylab=ylab,
			reducedDims=TRUE, ...)
	} else {
		if ( missing(xlab) )
			xlab <- NULL
		if ( missing(ylab) )
			ylab <- "Variances"
		panel_grid(c(1L,1L))
		screeplot(x@model, main="", ...)
		title(xlab=xlab, ylab=ylab, outer=TRUE)
	}
})

setMethod("image", c(x = "SpatialFastmap"),
	function(x, type = "x", ...)
{
	type <- match.arg(type)
	callNextMethod(x, y=x$x, ...)
})


.spatialRowDistFun <- function(x, y, neighbors,
	neighbor.weights = NULL, metric = "euclidean", p = 2, weights = NULL,
	verbose = NA, chunkopts = list(),
	BPPARAM = getCardinalBPPARAM(), ...)
{
	function(i) {
		if ( isTRUE(verbose) )
			message("calculating distances from index: ", paste0(i, collapse=" "))
		if ( is.null(neighbor.weights) ) {
			wts <- rep.int(1, length(neighbors))
		} else {
			wts <- rep_len(neighbor.weights, length(neighbors))
		}
		d <- rowDists(y, x[i,,drop=FALSE],
			metric=metric, p=p, weights=weights,
			verbose=isTRUE(verbose), chunkopts=chunkopts,
			BPPARAM=BPPARAM)
		if ( length(i) > 1L ) {
			FUN <- function(nbi, wi) colSums(wi * d[nbi,]) / sum(wi)
			t(mapply(FUN, neighbors, wts))
		} else {
			FUN <- function(nbi, wi) sum(wi * d[nbi]) / sum(wi)
			mapply(FUN, neighbors, wts)
		}
	}
}

.spatialColDistFun <- function(x, y, neighbors,
	neighbor.weights = NULL, metric = "euclidean", p = 2, weights = NULL,
	verbose = NA, chunkopts = list(),
	BPPARAM = getCardinalBPPARAM(), ...)
{
	function(i) {
		if ( isTRUE(verbose) )
			message("calculating distances from index: ", paste0(i, collapse=" "))
		if ( is.null(neighbor.weights) ) {
			stop("spatially-aware weights are NULL")
		} else {
			wts <- rep_len(neighbor.weights, length(neighbors))
		}
		d <- colDists(y, x[,i,drop=FALSE],
			metric=metric, p=p, weights=weights,
			verbose=isTRUE(verbose), chunkopts=chunkopts,
			BPPARAM=BPPARAM)
		if ( length(i) > 1L ) {
			FUN <- function(nbi, wi) colSums(wi * d[nbi,]) / sum(wi)
			t(mapply(FUN, neighbors, wts))
		} else {
			FUN <- function(nbi, wi) sum(wi * d[nbi]) / sum(wi)
			mapply(FUN, neighbors, wts)
		}
	}
}
