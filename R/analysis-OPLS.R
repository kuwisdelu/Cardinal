
### implement methods for OPLS ####

setMethod("OPLS", signature = c(x = "SImageSet", y = "matrix"), 
	function(x, y, ncomp = 20,
		method = "nipals",
		scale = FALSE,
		iter.max = 100, ...)
{
	method <- match.arg(method)
	ncomps <- sort(ncomp)
	if ( max(ncomps) > nrow(x) )
		.stop("OPLS: Can't fit more components than extent of dataset")
	.time.start()
	.message("OPLS: Centering data.")
	Xt <- as.matrix(iData(x))
	Xt <- scale(t(Xt), scale=scale)
	Y <- scale(y, scale=scale)
	if ( scale ) {
		scale <- attr(Xt, "scaled:scale")
		Yscale <- attr(Y, "scaled:scale")
	} else {
		Yscale <- rep(1, nrow(Y))
	}
	center <- attr(Xt, "scaled:center")
	Ycenter <- attr(Y, "scaled:center")
	.message("OPLS: Fitting orthogonal partial least squares components.")
	fit <- .OPLS.fit(Xt, Y, ncomp=max(ncomps), method=method, iter.max=iter.max)
	out <- lapply(ncomps, function(ncomp) {
		res <- append(fit, list(y=y, ncomp=ncomp,
			method=method, scale=scale, center=center,
			Yscale=Yscale, Ycenter=Ycenter))
		class(res) <- "ResultData"
		res
	})
	par <- AnnotatedDataFrame(
		data=data.frame(ncomp=sapply(out, function(fit) fit$ncomp)),
		varMetadata=data.frame(
			labelDescription=c(ncomp="Number of PLS Components")))
	featureNames(par) <- formatParam(pData(par))
	names(out) <- formatParam(pData(par))
	object <- new("OPLS",
		pixelData=x@pixelData,
		featureData=x@featureData,
		experimentData=x@experimentData,
		protocolData=x@protocolData,
		resultData=out,
		modelData=par)
	.time.stop()
	predict(object, newx=x)
})

setMethod("OPLS", signature = c(x = "SImageSet", y = "numeric"), 
	function(x, y,  ...)
{
	OPLS(x, as.matrix(y), ...)
})

setMethod("OPLS", signature = c(x = "SImageSet", y = "factor"), 
	function(x, y, ...)
{
	y <- sapply(levels(y), function(Ck) as.integer(y == Ck))
	OPLS(x, y, ...)
})

setMethod("OPLS", signature = c(x = "SImageSet", y = "character"), 
	function(x, y, ...)
{
	OPLS(x, factor(y), ...)
})

setMethod("predict", "OPLS",
	function(object, newx, newy, ...)
{
	if ( !is(newx, "iSet") )
		.stop("'newx' must inherit from 'iSet'")
	.time.start()
	Xt <- as.matrix(iData(newx))
	Xt <- scale(t(Xt), scale=object$scale[[1]])
	Y <- object$y[[1]]
	if ( missing(newy) ) {
		missing.newy <- TRUE
	} else {
		missing.newy <- FALSE
	}
	out <- lapply(object@resultData, function(res) {
		.message("OPLS: Predicting for ncomp = ", res$ncomp, ".")
		pred <- .OPLS.predict(Xt, Y, ncomp=res$ncomp, method=res$method,
			Oloadings=res$Oloadings, Oweights=res$Oweights)
		pred$fitted <- res$Yscale * pred$fitted + res$Ycenter
		res[names(pred)] <- pred
		if ( !missing.newy )
			res$y <- newy
		class(res) <- "ResultData"
		res
	})
	.message("PLS: Done.")
	.time.stop()
	new("PLS",
		pixelData=newx@pixelData,
		featureData=newx@featureData,
		experimentData=newx@experimentData,
		protocolData=newx@protocolData,
		resultData=out,
		modelData=object@modelData)
})

.OPLS.fit <- function(X, Y, ncomp, method, iter.max) {
	if ( method == "nipals" ) {
		nipals.OPLS(X, Y, ncomp=ncomp, iter.max=iter.max)
	} else {
		stop("OPLS method ", method, " not found")
	}
}

.OPLS.predict <- function(X, Y, ncomp, method, Oloadings, Oweights, iter.max) {
	Oloadings <- Oloadings[,1:ncomp,drop=FALSE]
	Oweights <- Oweights[,1:ncomp,drop=FALSE]
	Oscores <- X %*% Oweights
	Xortho <- tcrossprod(Oscores, Oloadings)
	Xnew <- X - Xortho
	if ( method == "nipals" ) {
		fit <- nipals.PLS(Xnew, Y, ncomp=1)
	} else {
		stop("PLS method ", method, " not found")
	}
	append(fit, list(Xnew=Xnew, Xortho=Xortho, Oscores=Oscores,
		Oloadings=Oloadings, Oweights=Oweights))
}


