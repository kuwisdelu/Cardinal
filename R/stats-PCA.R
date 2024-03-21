
#### Principal components analysis ####
## ------------------------------------

setMethod("PCA", "SpectralImagingExperiment", 
	function(x, ncomp = 3,
		center = TRUE, scale = FALSE,
		BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( length(processingData(x)) > 0L )
		warning("pending processing steps will be ignored")
	if ( length(ncomp) > 1L )
		ncomp <- max(ncomp)
	ans <- prcomp_lanczos(spectra(x), k=ncomp,
		center=center, scale.=scale, transpose=TRUE,
		nchunks=getCardinalNChunks(),
		verbose=getCardinalVerbose(),
		BPPARAM=getCardinalBPPARAM(), ...)
	if ( getCardinalVerbose() )
		message("returning principal components")
	as(SpatialResults(ans, x), "SpatialPCA")
})

setMethod("predict", "SpatialPCA",
	function(object, newdata, BPPARAM = getCardinalBPPARAM(), ...)
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
			BPPARAM=BPPARAM)
	} else {
		x <- spectra(newdata)
	}
	crossprod(x, object$rotation)
})

