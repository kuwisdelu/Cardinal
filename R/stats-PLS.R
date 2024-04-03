
#### Projection to latent structures ####
## --------------------------------------

setMethod("PLS", "ANY", 
	function(x, y, ncomp = 3,
		method = c("nipals", "simpls", "kernel1", "kernel2"),
		center = TRUE, scale = FALSE,
		nchunks = getCardinalNChunks(),
		verbose = getCardinalVerbose(),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	method <- match.arg(method)
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
			message(msg, "using SIMPLS")
		ans <- pls_kernel(x, y=y, k=max(ncomp), method=1L,
			center=center, scale.=scale,
			nchunks=nchunks, verbose=verbose,
			BPPARAM=BPPARAM, ...)
	} else if ( method == "kernel2" ) {
		if ( verbose )
			message(msg, "using SIMPLS")
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
	ans <- object$fitted.values
	if ( type == "class" ) {
		cls <- apply(ans, 1L, which.max)
		ans <- factor(cls,
			levels=seq_len(ncol(ans)),
			labels=colnames(ans))
	}
	ans
})

setMethod("predict", "SpatialPLS",
	function(object, newdata, ncomp,
		type = c("response", "class"), simplify = TRUE, ...)
{
	if ( !missing(newdata) && !is(newdata, "SpectralImagingExperiment") )
		stop("'newdata' must inherit from 'SpectralImagingExperiment'")
	type <- match.arg(type)
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

#### Orthogonal projection to latent structures ####
## -------------------------------------------------

setMethod("OPLS", "ANY", 
	function(x, y, ncomp = 3,
		center = TRUE, scale = FALSE, retx = TRUE,
		nchunks = getCardinalNChunks(),
		verbose = getCardinalVerbose(),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( verbose )
		message("preprocessing data to remove orthogonal variation")
	ans <- opls_nipals(x, y=y, k=max(ncomp),
		center=center, scale.=scale,
		nchunks=nchunks, verbose=verbose,
		BPPARAM=BPPARAM, ...)
	fit <- lapply(setNames(ncomp, paste0("ncomp=", ncomp)),
		function(k) {
			if ( verbose ) {
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
				nchunks=nchunks, verbose=verbose,
				BPPARAM=BPPARAM, ...)
		})
	nms <- c("coefficients", "residuals", "fitted.values")
	ans$ncomp <- ncomp
	ans$regressions <- fit
	ans[nms] <- fit[[which.max(ncomp)]][nms]
	if ( !retx )
		ans$x <- NULL
	if ( verbose )
		message("returning projection to latent structures")
	ans
})

setMethod("OPLS", "SpectralImagingExperiment", 
	function(x, y, ncomp = 3,
		center = TRUE, scale = FALSE, retx = TRUE, ...)
{
	if ( length(processingData(x)) > 0L )
		warning("pending processing steps will be ignored")
	ans <- OPLS(spectra(x), y=y, ncomp=ncomp,
		center=center, scale=scale, transpose=TRUE, ...)
	as(SpatialResults(ans, x), "SpatialOPLS")
})

setMethod("fitted", "SpatialOPLS",
	function(object, type = c("response", "class"), ...)
{
	type <- match.arg(type)
	ans <- object$fitted.values
	if ( type == "class" ) {
		cls <- apply(ans, 1L, which.max)
		ans <- factor(cls,
			levels=seq_len(ncol(ans)),
			labels=colnames(ans))
	}
	ans
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
				fitted(fit, type=type, ...)
			}
		}, newdata=newdata)
	names(ans) <- paste0("ncomp=", ncomp)
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

setMethod("topFeatures", "SpatialOPLS",
	function(object, n = Inf, sort.by = c("vip", "coefficients"), ...)
{
	sort.by <- match.arg(sort.by)
	vips <- vip(object$regressions[[which.max(object$ncomp)]])
	coefs <- coef(object@model)
	resp <- rep(colnames(coefs), each=nrow(coefs))
	topf <- DataFrame(response=resp, vip=vips, coefficients=as.vector(coefs))
	topf <- .rank_featureData(object, topf, sort.by)
	head(topf, n=n)
})

