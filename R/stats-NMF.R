
#### Nonnegative matrix factorization ####
## ---------------------------------------

setMethod("NMF", "ANY",
	function(x, ncomp = 3, method = c("als", "mult"),
		verbose = getCardinalVerbose(), ...)
{
	method <- match.arg(method)
	msg <- "estimating nonnegative matrix factorization "
	if ( method == "als" ) {
		if ( verbose )
			message(msg, "using alternating least squares")
		ans <- nnmf_als(x, k=max(ncomp), verbose=verbose, ...)
	} else if ( method == "mult" ) {
		if ( verbose )
			message(msg, "using multiplicative updates")
		ans <- nnmf_mult(x, k=max(ncomp), verbose=verbose, ...)
	} else {
		stop("unsupported method: ", method)
	}
	if ( verbose )
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

setMethod("plot", c(x = "SpatialNMF", y = "missing"),
	function(x, type = c("activation", "x"), ..., xlab, ylab)
{
	type <- match.arg(type)
	if ( type == "activation" ) {
		if ( missing(xlab) )
			xlab <- NULL
		if ( missing(ylab) )
			ylab <- "Loadings"
		callNextMethod(x, y=x$activation, xlab=xlab, ylab=ylab, ...)
	} else {
		callNextMethod(x, y=x$x, xlab=xlab, ylab=ylab,
			reducedDims=TRUE, ...)
	}
})

setMethod("image", c(x = "SpatialNMF"),
	function(x, type = "x", ...)
{
	type <- match.arg(type)
	callNextMethod(x, y=x$x, ...)
})

