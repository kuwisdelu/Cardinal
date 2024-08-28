
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
		nbwts <- spatialWeights(x=x,
			coord=coord, r=r, byrow=!transpose,
			weights=weights, neighbors=neighbors,
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
		neighbors=neighbors, weights=weights,
		transpose=TRUE, ...)
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
		nbwts <- spatialWeights(newdata, r=object$r,
			neighbors=neighbors, weights=weights,
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

