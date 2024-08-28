
#### Spatially-aware FastMap ####
## ------------------------------

setMethod("spatialFastmap", "ANY",
	function(x, coord, r = 1, ncomp = 3,
		weights = c("gaussian", "adaptive"),
		neighbors = findNeighbors(coord, r=r),
		transpose = TRUE, niter = 10L,
		verbose = getCardinalVerbose(), chunkopts = list(),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( "method" %in% ...names() ) {
		.Deprecated(old="method", new="weights")
		weights <- list(...)$method
	}
	if ( is.character(weights) ) {
		.Log("computing ", weights, " weights",
			message=verbose)
		nbwts <- spatialWeights(x=x, byrow=!transpose,
			coord=coord, r=r, neighbors=neighbors,
			verbose=verbose, chunkopts=chunkopts,
			BPPARAM=BPPARAM)
	} else {
		.Log("using custom weights",
			message=verbose)
		nbwts <- rep_len(weights, length(neighbors))
		weights <- "custom"
	}
	if ( transpose ) {
		distfun <- .spatialColDists
	} else {
		distfun <- .spatialRowDists
	}
	ncomp <- min(ncomp, dim(x))
	ans <- fastmap(x, k=ncomp, distfun=distfun,
		neighbors=neighbors, neighbors.weights=nbwts,
		transpose=transpose, niter=niter,
		verbose=verbose, chunkopts=chunkopts,
		BPPARAM=BPPARAM, ...)
	ans$weights <- weights
	ans$r <- r
	.Log("returning FastMap projection",
		message=verbose)
	ans
})

setMethod("spatialFastmap", "SpectralImagingExperiment", 
	function(x, r = 1, ncomp = 3,
		weights = c("gaussian", "adaptive"),
		neighbors = findNeighbors(x, r=r), ...)
{
	if ( length(processingData(x)) > 0L )
		.Warn("pending processing steps will be ignored")
	ans <- spatialFastmap(spectra(x),
		coord=coord(x), r=r, ncomp=ncomp,
		neighbors=neighbors, weights=weights, transpose=TRUE, ...)
	as(SpatialResults(ans, x), "SpatialFastmap")
})

setMethod("predict", "SpatialFastmap",
	function(object, newdata,
		weights = object$weights,
		neighbors = findNeighbors(newdata, r=object$r),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( !is(newdata, "SpectralImagingExperiment") )
		.Error("'newdata' must inherit from 'SpectralImagingExperiment'")
	if ( nrow(newdata) != nrow(object$pivot.array) )
		.Error("'newdata' does not have the correct number of dimensions")
	if ( length(processingData(newdata)) > 0L )
		.Warn("pending processing steps will be ignored")
	if ( is.character(weights) ) {
		nbwts <- spatialWeights(spectra(newdata), byrow=FALSE,
			coord=coord(newdata), r=object$r, neighbors=neighbors,
			BPPARAM=BPPARAM, ...)
	} else {
		nbwts <- rep_len(weights, length(neighbors))
	}
	predict(object@model, newdata=spectra(newdata),
		neighbors=neighbors, neighbors.weights=nbwts,
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
		.Log("calculating distances from index: ", paste0(i, collapse=" "),
			message=isTRUE(verbose))
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
			FUN <- function(nbi, wi) colSums(wi * d[nbi,,drop=FALSE]) / sum(wi)
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
		.Log("calculating distances from index: ", paste0(i, collapse=" "),
			message=isTRUE(verbose))
		if ( is.null(neighbor.weights) ) {
			.Error("spatially-aware weights are NULL")
		} else {
			wts <- rep_len(neighbor.weights, length(neighbors))
		}
		d <- colDists(y, x[,i,drop=FALSE],
			metric=metric, p=p, weights=weights,
			verbose=isTRUE(verbose), chunkopts=chunkopts,
			BPPARAM=BPPARAM)
		if ( length(i) > 1L ) {
			FUN <- function(nbi, wi) colSums(wi * d[nbi,,drop=FALSE]) / sum(wi)
			t(mapply(FUN, neighbors, wts))
		} else {
			FUN <- function(nbi, wi) sum(wi * d[nbi]) / sum(wi)
			mapply(FUN, neighbors, wts)
		}
	}
}
