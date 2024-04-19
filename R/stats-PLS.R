
#### Projection to latent structures ####
## --------------------------------------

setMethod("PLS", "ANY", 
	function(x, y, ncomp = 3,
		method = c("nipals", "simpls", "kernel1", "kernel2"),
		center = TRUE, scale = FALSE, bags = NULL,
		nchunks = getCardinalNChunks(),
		verbose = getCardinalVerbose(),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	method <- match.arg(method)
	if ( !is.null(bags) )
	{
		return(mi_learn(PLS, x=x, y=y, ncomp=ncomp,
			method=method, bags=bags, score=fitted,
			center=center, scale=scale,
			nchunks=nchunks, verbose=verbose,
			BPPARAM=BPPARAM, ...))
	}
	msg <- "projecting to latent structures "
	if ( method == "nipals" ) {
		if ( verbose )
			message(msg, "using NIPALS")
		ans <- pls_nipals(x, y=y, k=max(ncomp),
			center=center, scale.=scale,
			nchunks=nchunks, verbose=verbose,
			BPPARAM=BPPARAM, ...)
	} else if ( method == "simpls" ) {
		if ( verbose )
			message(msg, "using SIMPLS")
		ans <- pls_simpls(x, y=y, k=max(ncomp),
			center=center, scale.=scale,
			nchunks=nchunks, verbose=verbose,
			BPPARAM=BPPARAM, ...)
	} else if ( method == "kernel1" ) {
		if ( verbose )
			message(msg, "using kernel #1")
		ans <- pls_kernel(x, y=y, k=max(ncomp), method=1L,
			center=center, scale.=scale,
			nchunks=nchunks, verbose=verbose,
			BPPARAM=BPPARAM, ...)
	} else if ( method == "kernel2" ) {
		if ( verbose )
			message(msg, "using kernel #2")
		ans <- pls_kernel(x, y=y, k=max(ncomp), method=2L,
			center=center, scale.=scale,
			nchunks=nchunks, verbose=verbose,
			BPPARAM=BPPARAM, ...)
	} else {
		stop("unsupported method: ", method)
	}
	if ( verbose )
		message("returning projection to latent structures")
	ans
})

setMethod("PLS", "SpectralImagingExperiment", 
	function(x, y, ncomp = 3,
		method = c("nipals", "simpls", "kernel1", "kernel2"),
		center = TRUE, scale = FALSE, ...)
{
	if ( length(processingData(x)) > 0L )
		warning("pending processing steps will be ignored")
	ans <- PLS(spectra(x), y=y, ncomp=ncomp,
		center=center, scale=scale, transpose=TRUE, ...)
	as(SpatialResults(ans, x), "SpatialPLS")
})

setMethod("fitted", "SpatialPLS",
	function(object, type = c("response", "class"), ...)
{
	type <- match.arg(type)
	fitted(object@model, type=type)
})

setMethod("predict", "SpatialPLS",
	function(object, newdata, ncomp,
		type = c("response", "class"), simplify = TRUE, ...)
{
	type <- match.arg(type)
	if ( !missing(newdata) && !is(newdata, "SpectralImagingExperiment") )
		stop("'newdata' must inherit from 'SpectralImagingExperiment'")
	if ( !missing(newdata) ) {
		if ( length(processingData(newdata)) > 0L )
			warning("pending processing steps will be ignored")
		if ( missing(ncomp) )
			ncomp <- ncol(object$loadings)
		ans <- predict(object@model, newdata=spectra(newdata), k=ncomp,
			type=type, simplify=FALSE, ...)
		names(ans) <- paste0("ncomp=", ncomp)
		if ( simplify ) {
			if ( length(ans) > 1L ) {
				if ( type == "class" ) {
					as.data.frame(ans, check.names=FALSE)
				} else {
					simplify2array(ans)
				}
			} else {
				ans[[1L]]
			}
		} else {
			ans
		}
	} else {
		fitted(object@model, type=type, ...)
	}
})

setMethod("topFeatures", "SpatialPLS",
	function(object, n = Inf, sort.by = c("vip", "coefficients"), ...)
{
	sort.by <- match.arg(sort.by)
	vips <- vip(object@model)
	coefs <- coef(object@model)
	resp <- rep(colnames(coefs), each=nrow(coefs))
	topf <- DataFrame(response=resp, vip=vips, coefficients=as.vector(coefs))
	topf <- .rank_featureData(object, topf, sort.by)
	head(topf, n=n)
})

setMethod("plot", c(x = "SpatialPLS", y = "missing"),
	function(x, type = c("coefficients", "vip", "scores"), ..., xlab, ylab)
{
	type <- match.arg(type)
	if ( missing(xlab) )
		xlab <- NULL
	if ( type == "vip" ) {
		if ( missing(ylab) )
			ylab <- "Coefficients"
		callNextMethod(x, y=coef(x), xlab=xlab, ylab=ylab, ...)
	} else if ( type == "vip" ) {
		if ( missing(ylab) )
			ylab <- "Importance"
		callNextMethod(x, y=vip(x), xlab=xlab, ylab=ylab, ...)
	} else {
		callNextMethod(x, y=x$scores, xlab=xlab, ylab=ylab,
			reducedDims=TRUE, ...)
	}
})

setMethod("image", c(x = "SpatialPLS"),
	function(x, type = c("response", "class"), ...)
{
	type <- match.arg(type)
	callNextMethod(x, y=fitted(x, type=type), ...)
})


#### Orthogonal projection to latent structures ####
## -------------------------------------------------

setMethod("OPLS", "ANY", 
	function(x, y, ncomp = 3, retx = TRUE,
		center = TRUE, scale = FALSE, bags = NULL,
		nchunks = getCardinalNChunks(),
		verbose = getCardinalVerbose(),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( !is.null(bags) )
	{
		return(mi_learn(OPLS, x=x, y=y, ncomp=ncomp,
			retx=retx, bags=bags, score=fitted,
			center=center, scale=scale,
			nchunks=nchunks, verbose=verbose,
			BPPARAM=BPPARAM, ...))
	}
	if ( verbose )
		message("preprocessing data to remove orthogonal variation")
	ans <- opls_nipals(x, y=y, k=max(ncomp),
		center=center, scale.=scale, regression=TRUE,
		nchunks=nchunks, verbose=verbose,
		BPPARAM=BPPARAM, ...)
	if ( !retx )
		ans$x <- NULL
	if ( verbose )
		message("returning projection to latent structures")
	ans
})

setMethod("OPLS", "SpectralImagingExperiment", 
	function(x, y, ncomp = 3, retx = FALSE,
		center = TRUE, scale = FALSE, ...)
{
	if ( length(processingData(x)) > 0L )
		warning("pending processing steps will be ignored")
	ans <- OPLS(spectra(x), y=y, ncomp=ncomp,
		center=center, scale=scale, retx=retx, transpose=TRUE, ...)
	as(SpatialResults(ans, x), "SpatialOPLS")
})

setMethod("coef", "SpatialOPLS",
	function(object, ...) coef(object@model, ...))

setMethod("residuals", "SpatialOPLS",
	function(object, ...) residuals(object@model, ...))

setMethod("fitted", "SpatialOPLS",
	function(object, type = c("response", "class"), ...)
{
	type <- match.arg(type)
	fitted(object@model, type=type)
})

setMethod("predict", "SpatialOPLS",
	function(object, newdata, ncomp,
		type = c("response", "class"), simplify = TRUE, ...)
{
	type <- match.arg(type)
	if ( !missing(newdata) && !is(newdata, "SpectralImagingExperiment") )
		stop("'newdata' must inherit from 'SpectralImagingExperiment'")
	if ( !missing(newdata) ) {
		if ( length(processingData(newdata)) > 0L )
			warning("pending processing steps will be ignored")
		if ( missing(ncomp) )
			ncomp <- ncol(object$loadings)
		ans <- predict(object@model, newdata=spectra(newdata), k=ncomp,
			type=type, simplify=FALSE, ...)
		names(ans) <- paste0("ncomp=", ncomp)
		if ( simplify ) {
			if ( length(ans) > 1L ) {
				if ( type == "class" ) {
					as.data.frame(ans, check.names=FALSE)
				} else {
					simplify2array(ans)
				}
			} else {
				ans[[1L]]
			}
		} else {
			ans
		}
	} else {
		fitted(object@model, type=type, ...)
	}
})

setMethod("topFeatures", "SpatialOPLS",
	function(object, n = Inf, sort.by = c("vip", "coefficients"), ...)
{
	sort.by <- match.arg(sort.by)
	k <- length(object$regressions)
	vips <- vip(object$regressions[[k]])
	coefs <- coef(object@model)
	resp <- rep(colnames(coefs), each=nrow(coefs))
	topf <- DataFrame(response=resp, vip=vips, coefficients=as.vector(coefs))
	topf <- .rank_featureData(object, topf, sort.by)
	head(topf, n=n)
})

setMethod("plot", c(x = "SpatialOPLS", y = "missing"),
	function(x, type = c("coefficients", "vip", "scores"), ..., xlab, ylab)
{
	type <- match.arg(type)
	if ( missing(xlab) )
		xlab <- NULL
	if ( type == "vip" ) {
		if ( missing(ylab) )
			ylab <- "Coefficients"
		callNextMethod(x, y=coef(x), xlab=xlab, ylab=ylab, ...)
	} else if ( type == "vip" ) {
		if ( missing(ylab) )
			ylab <- "Importance"
		callNextMethod(x, y=vip(x), xlab=xlab, ylab=ylab, ...)
	} else {
		callNextMethod(x, y=x$scores, xlab=xlab, ylab=ylab,
			reducedDims=TRUE, ...)
	}
})

setMethod("image", c(x = "SpatialOPLS"),
	function(x, type = c("response", "class"), ...)
{
	callNextMethod(x, y=fitted(x, type=type), ...)
})

