
#### Projection to latent structures ####
## --------------------------------------

setMethod("PLS", "ANY", 
	function(x, y, ncomp = 3,
		method = c("nipals", "simpls", "kernel1", "kernel2"),
		center = TRUE, scale = FALSE,
		BPPARAM = getCardinalBPPARAM(), ...)
{
	method <- match.arg(method)
	msg <- "projecting to latent structures "
	if ( method == "nipals" ) {
		if ( getCardinalVerbose() )
			message(msg, "using NIPALS")
		ans <- pls_nipals(x, y=y, k=max(ncomp),
			center=center, scale.=scale,
			verbose=getCardinalVerbose(),
			nchunks=getCardinalNChunks(),
			BPPARAM=BPPARAM, ...)
	} else if ( method == "simpls" ) {
		if ( getCardinalVerbose() )
			message(msg, "using SIMPLS")
		ans <- pls_simpls(x, y=y, k=max(ncomp),
			center=center, scale.=scale,
			verbose=getCardinalVerbose(),
			nchunks=getCardinalNChunks(),
			BPPARAM=BPPARAM, ...)
	} else if ( method == "kernel1" ) {
		if ( getCardinalVerbose() )
			message(msg, "using SIMPLS")
		ans <- pls_kernel(x, y=y, k=max(ncomp), method=1L,
			center=center, scale.=scale,
			verbose=getCardinalVerbose(),
			nchunks=getCardinalNChunks(),
			BPPARAM=BPPARAM, ...)
	} else if ( method == "kernel2" ) {
		if ( getCardinalVerbose() )
			message(msg, "using SIMPLS")
		ans <- pls_kernel(x, y=y, k=max(ncomp), method=2L,
			center=center, scale.=scale,
			verbose=getCardinalVerbose(),
			nchunks=getCardinalNChunks(),
			BPPARAM=BPPARAM, ...)
	} else {
		stop("unsupported method: ", method)
	}
	if ( getCardinalVerbose() )
		message("returning projection to latent structures")
	ans
})

setMethod("PLS", "SpectralImagingExperiment", 
	function(x, y, ncomp = 3,
		method = c("nipals", "simpls", "kernel1", "kernel2"),
		center = TRUE, scale = FALSE,
		BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( length(processingData(x)) > 0L )
		warning("pending processing steps will be ignored")
	ans <- PLS(spectra(x), y=y, ncomp=ncomp,
		center=center, scale=scale, transpose=TRUE,
		BPPARAM=BPPARAM, ...)
	as(SpatialResults(ans, x), "SpatialPLS")
})

setMethod("predict", "SpatialPLS",
	function(object, newdata, ncomp,
		type = c("response", "class"), simplify = TRUE, ...)
{
	if ( !missing(newdata) && !is(newdata, "SpectralImagingExperiment") )
		stop("'newdata' must inherit from 'SpectralImagingExperiment'")
	if ( !missing(newdata) ) {
		if ( length(processingData(newdata)) > 0L )
			warning("pending processing steps will be ignored")
		predict(object@model, newdata=spectra(newdata), k=ncomp,
			type=type, simplify=simplify, ...)
	} else {
		predict(object@model, type=type, ...)
	}
})

#### Orthogonal projection to latent structures ####
## -------------------------------------------------

setMethod("OPLS", "ANY", 
	function(x, y, ncomp = 3,
		center = TRUE, scale = FALSE, retx = TRUE,
		BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( getCardinalVerbose() )
		message("preprocessing data to remove orthogonal variation")
	ans <- opls_nipals(x, y=y, k=max(ncomp),
		center=center, scale.=scale,
		verbose=getCardinalVerbose(),
		nchunks=getCardinalNChunks(),
		BPPARAM=BPPARAM, ...)
	fit <- lapply(setNames(ncomp, paste0("C", ncomp)),
		function(k) {
			if ( getCardinalVerbose() ) {
				label <- if (k != 1L) "components" else "component"
				message("using data with ", k,
					" orthogonal ", label, " removed")
			}
			if ( k != ncol(ans$loadings) ) {
				xk <- predict(ans, newdata=x, k=k)
			} else {
				xk <- ans$x
			}
			pls_nipals(xk, y=y, k=1L,
				center=FALSE, scale.=FALSE,
				verbose=getCardinalVerbose(),
				nchunks=getCardinalNChunks(),
				BPPARAM=BPPARAM, ...)
		})
	nms <- c("coefficients", "residuals", "fitted.values")
	ans$ncomp <- ncomp
	ans$regressions <- fit
	ans[nms] <- fit[[which.max(ncomp)]][nms]
	if ( !retx )
		ans$x <- NULL
	if ( getCardinalVerbose() )
		message("returning projection to latent structures")
	ans
})

setMethod("OPLS", "SpectralImagingExperiment", 
	function(x, y, ncomp = 3,
		center = TRUE, scale = FALSE, retx = TRUE,
		BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( length(processingData(x)) > 0L )
		warning("pending processing steps will be ignored")
	ans <- OPLS(spectra(x), y=y, ncomp=ncomp,
		center=center, scale=scale, transpose=TRUE,
		BPPARAM=BPPARAM, ...)
	as(SpatialResults(ans, x), "SpatialOPLS")
})

setMethod("predict", "SpatialOPLS",
	function(object, newdata, ncomp,
		type = c("response", "class"), simplify = TRUE, ...)
{
	if ( !missing(newdata) && !is(newdata, "SpectralImagingExperiment") )
		stop("'newdata' must inherit from 'SpectralImagingExperiment'")
	if ( missing(ncomp) )
		ncomp <- max(object$ncomp)
	if ( !all(ncomp %in% object$ncomp) )
		stop("can only predict for ncomp = ",
			paste0(object$ncomp, collapse=", "))
	type <- match.arg(type)
	ans <- lapply(ncomp,
		function(k, newdata) {
			fit <- object$regressions[[which(object$ncomp %in% k)[1L]]]
			if ( !missing(newdata) ) {
				if ( length(processingData(newdata)) > 0L )
					warning("pending processing steps will be ignored")
				xk <- predict(object@model, newdata=spectra(newdata), k=k)
				predict(fit, newdata=xk, type=type, simplify=TRUE, ...)
			} else {
				predict(fit, type=type, ...)
			}
		}, newdata=newdata)
	if ( simplify ) {
		if ( length(ans) > 1L ) {
			if ( type == "class" ) {
				as.data.frame(ans)
			} else {
				simplify2array(ans)
			}
		} else {
			ans[[1L]]
		}
	} else {
		ans
	}
})

