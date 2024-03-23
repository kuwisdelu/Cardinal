
#### Spatially-aware FastMap ####
## ------------------------------

setMethod("spatialFastmap", "ANY",
	function(x, coord, r = 1, ncomp = 3,
		neighbors = findNeighbors(coord, r=r),
		weights = c("gaussian", "adaptive"),
		transpose = FALSE, niter = 3L,
		BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( is.character(weights) ) {
		if ( getCardinalVerbose() )
			message("calculating gaussian weights")
		wts <- spatialWeights(as.matrix(coord), neighbors=neighbors)
		if ( match.arg(weights) == "adaptive" )
		{
			if ( getCardinalVerbose() )
				message("calculating adaptive weights")
			awts <- spatialWeights(x, neighbors=neighbors,
				weights="adaptive", byrow=!transpose,
				nchunks=getCardinalNChunks(),
				verbose=getCardinalVerbose(),
				BPPARAM=BPPARAM, ...)
			wts <- Map("*", wts, awts)
		}
	} else {
		wts <- rep_len(weights, length(neighbors))
		weights <- "custom"
	}
	if ( transpose ) {
		distfun <- .spatialColDistFun
	} else {
		distfun <- .spatialRowDistFun
	}
	ncomp <- min(ncomp, dim(x))
	ans <- fastmap(x, k=ncomp, distfun=distfun,
		neighbors=neighbors, weights=wts,
		transpose=transpose, niter=niter,
		nchunks=getCardinalNChunks(),
		verbose=getCardinalVerbose(),
		BPPARAM=BPPARAM, ...)
	ans$weights <- weights
	ans$r <- r
	if ( getCardinalVerbose() )
		message("returning FastMap projection")
	ans
})

setMethod("spatialFastmap", "SpectralImagingExperiment", 
	function(x, r = 1, ncomp = 3,
		neighbors = findNeighbors(x, r=r),
		weights = c("gaussian", "adaptive"),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( length(processingData(x)) > 0L )
		warning("pending processing steps will be ignored")
	ans <- spatialFastmap(spectra(x),
		coord=coord(x), r=r, ncomp=ncomp,
		neighbors=neighbors, weights=weights, transpose=TRUE,
		BPPARAM=BPPARAM, ...)
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
		awts <- spatialWeights(spectra(newdata), neighbors=neighbors,
			weights="adaptive", byrow=!object$transpose,
			nchunks=getCardinalNChunks(),
			verbose=FALSE,
			BPPARAM=BPPARAM, ...)
		wts <- Map("*", wts, awts)
	}
	predict(object@model, newdata=spectra(newdata),
		neighbors=neighbors, weights=wts,
		nchunks=getCardinalNChunks(),
		verbose=FALSE,
		BPPARAM=BPPARAM, ...)
})

.spatialRowDistFun <- function(x, y, neighbors,
	weights = NULL, metric = "euclidean", p = 2,
	verbose = NA, nchunks = NA, BPPARAM = bpparam(), ...)
{
	function(i) {
		if ( isTRUE(verbose) )
			message("calculating distances from index: ", i)
		if ( is.null(weights) ) {
			weights <- rep.int(1, length(neighbors))
		} else {
			weights <- rep_len(weights, length(neighbors))
		}
		d <- rowDists(y, x[i,,drop=FALSE], metric=metric, p=p,
			verbose=isTRUE(verbose), nchunks=nchunks,
			BPPARAM=BPPARAM)
		FUN <- function(nbi, wts) sum(wts * d[nbi]) / sum(wts)
		mapply(FUN, neighbors, weights)
	}
}

.spatialColDistFun <- function(x, y, neighbors,
	weights = NULL, metric = "euclidean", p = 2,
	verbose = NA, nchunks = NA, BPPARAM = bpparam(), ...)
{
	function(i) {
		if ( isTRUE(verbose) )
			message("calculating distances from index: ", i)
		if ( is.null(weights) ) {
			stop("spatially-aware weights are NULL")
		} else {
			weights <- rep_len(weights, length(neighbors))
		}
		d <- colDists(y, x[,i,drop=FALSE], metric=metric, p=p,
			verbose=isTRUE(verbose), nchunks=nchunks,
			BPPARAM=BPPARAM)
		FUN <- function(nbi, wts) sum(wts * d[nbi]) / sum(wts)
		mapply(FUN, neighbors, weights)
	}
}
