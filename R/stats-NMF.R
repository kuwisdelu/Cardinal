
#### Nonnegative matrix factorization ####
## ---------------------------------------

setMethod("NMF", "ANY",
	function(x, ncomp = 3, method = c("als", "mult"), ...)
{
	method <- match.arg(method)
	msg <- "estimating nonnegative matrix factorization "
	if ( method == "als" ) {
		if ( getCardinalVerbose() )
			message(msg, "using alternating least squares")
		ans <- nnmf_als(x, k=max(ncomp), verbose=getCardinalVerbose(), ...)
	} else if ( method == "mult" ) {
		if ( getCardinalVerbose() )
			message(msg, "using multiplicative updates")
		ans <- nnmf_mult(x, k=max(ncomp), verbose=getCardinalVerbose(), ...)
	} else {
		stop("unsupported method: ", method)
	}
	if ( getCardinalVerbose() )
		message("returning nonnegative matrix factorization")
	ans
})

setMethod("NMF", "SpectralImagingExperiment", 
	function(x, ncomp = 3, method = c("als", "mult"), ...)
{
	if ( length(processingData(x)) > 0L )
		warning("pending processing steps will be ignored")
	ans <- NMF(spectra(x), ncomp=ncomp, method=method, transpose=TRUE, ...)
	as(SpatialResults(ans, x), "SpatialNMF")
})

setMethod("predict", "SpatialNMF",
	function(object, newdata, ...)
{
	if ( !is(newdata, "SpectralImagingExperiment") )
		stop("'newdata' must inherit from 'SpectralImagingExperiment'")
	if ( length(processingData(newdata)) > 0L )
		warning("pending processing steps will be ignored")
	predict(object@model, newdata=spectra(newdata), ...)
})

