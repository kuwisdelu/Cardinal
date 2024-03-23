
#### Principal components analysis ####
## ------------------------------------

setMethod("PCA", "ANY", 
	function(x, ncomp = 3,
		center = TRUE, scale = FALSE,
		BPPARAM = getCardinalBPPARAM(), ...)
{
	ans <- prcomp_lanczos(x, k=max(ncomp),
		center=center, scale.=scale,
		nchunks=getCardinalNChunks(),
		verbose=getCardinalVerbose(),
		BPPARAM=BPPARAM, ...)
	if ( getCardinalVerbose() )
		message("returning principal components")
	ans
})

setMethod("PCA", "SpectralImagingExperiment", 
	function(x, ncomp = 3,
		center = TRUE, scale = FALSE,
		BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( length(processingData(x)) > 0L )
		warning("pending processing steps will be ignored")
	ans <- PCA(spectra(x), ncomp=ncomp, transpose=TRUE,
		center=center, scale=scale, BPPARAM=BPPARAM, ...)
	as(SpatialResults(ans, x), "SpatialPCA")
})

setMethod("predict", "SpatialPCA",
	function(object, newdata,
		BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( !is(newdata, "SpectralImagingExperiment") )
		stop("'newdata' must inherit from 'SpectralImagingExperiment'")
	if ( length(processingData(newdata)) > 0L )
		warning("pending processing steps will be ignored")
	if ( nrow(newdata) != nrow(object$rotation) )
		stop("'newdata' does not have the correct number of dimensions")
	if ( (!isFALSE(object$center) || !isFALSE(object$scale)) ) {
		x <- rowscale(spectra(newdata),
			center=object$center, scale=object$scale,
			nchunks=getCardinalNChunks(),
			verbose=getCardinalVerbose(),
			BPPARAM=BPPARAM, ...)
	} else {
		x <- spectra(newdata)
	}
	crossprod(x, object$rotation)
})

