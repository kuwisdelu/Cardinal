
#### Principal components analysis ####
## ------------------------------------

setMethod("PCA", "ANY", 
	function(x, ncomp = 3,
		center = TRUE, scale = FALSE,
		verbose = getCardinalVerbose(), chunkopts = list(),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	ans <- prcomp_lanczos(x, k=max(ncomp),
		center=center, scale.=scale,
		verbose=verbose, chunkopts=chunkopts,
		BPPARAM=BPPARAM, ...)
	.Log("returning principal components",
		message=verbose)
	ans
})

setMethod("PCA", "SpectralImagingExperiment", 
	function(x, ncomp = 3,
		center = TRUE, scale = FALSE, ...)
{
	if ( length(processingData(x)) > 0L )
		.Warn("queued processing steps will be ignored")
	ans <- PCA(spectra(x), ncomp=ncomp, transpose=TRUE,
		center=center, scale=scale, ...)
	as(SpatialResults(ans, x), "SpatialPCA")
})

setMethod("predict", "SpatialPCA",
	function(object, newdata,
		BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( !is(newdata, "SpectralImagingExperiment") )
		.Error("'newdata' must inherit from 'SpectralImagingExperiment'")
	if ( length(processingData(newdata)) > 0L )
		.Warn("queued processing steps will be ignored")
	if ( nrow(newdata) != nrow(object$rotation) )
		.Error("'newdata' does not have the correct number of dimensions")
	if ( (!isFALSE(object$center) || !isFALSE(object$scale)) ) {
		x <- rowscale(spectra(newdata),
			center=object$center, scale=object$scale,
			verbose=FALSE, BPPARAM=BPPARAM, ...)
	} else {
		x <- spectra(newdata)
	}
	crossprod(x, object$rotation)
})

setMethod("plot", c(x = "SpatialPCA", y = "missing"),
	function(x, type = c("rotation", "scree", "x"), ..., xlab, ylab)
{
	type <- match.arg(type)
	if ( missing(xlab) )
		xlab <- NULL
	if ( type == "rotation" ) {
		if ( missing(ylab) )
			ylab <- "Loadings"
		callNextMethod(x, y=x$rotation, xlab=xlab, ylab=ylab, ...)
	} else if ( type == "x" ) {
		callNextMethod(x, y=x$x, xlab=xlab, ylab=ylab,
			reducedDims=TRUE, ...)
	} else {
		if ( missing(ylab) )
			ylab <- "Variances"
		panel_grid(c(1L,1L))
		screeplot(x@model, main="", ...)
		title(xlab=xlab, ylab=ylab, outer=TRUE)
	}
})

setMethod("image", c(x = "SpatialPCA"),
	function(x, type = "x", ...)
{
	type <- match.arg(type)
	callNextMethod(x, y=x$x, ...)
})

