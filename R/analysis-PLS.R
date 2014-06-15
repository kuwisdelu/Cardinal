
### implement methods for PLS ####

setMethod("PLS", signature = c(x = "SImageSet", y = "matrix"), 
	function(x, y, ncomp = 20,
		method = "nipals",
		scale = FALSE,
		iter.max = 100, ...)
{
	method <- match.arg(method)
	ncomps <- sort(ncomp)
	if ( max(ncomps) > nrow(x) )
		.stop("PLS: Can't fit more components than extent of dataset")
	.time.start()
	.message("PLS: Centering data.")
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
	.message("PLS: Fitting partial least squares components.")
	fit <- .PLS.fit(Xt, Y, ncomp=max(ncomps), method=method, iter.max=iter.max)
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
	object <- new("PLS",
		pixelData=x@pixelData,
		featureData=x@featureData,
		experimentData=x@experimentData,
		protocolData=x@protocolData,
		resultData=out,
		modelData=par)
	.time.stop()
	predict(object, newx=x)
})

setMethod("PLS", signature = c(x = "SImageSet", y = "numeric"), 
	function(x, y,  ...)
{
	PLS(x, as.matrix(y), ...)
})

setMethod("PLS", signature = c(x = "SImageSet", y = "factor"), 
	function(x, y, ...)
{
	y <- sapply(levels(y), function(Ck) as.integer(y == Ck))
	PLS(x, y, ...)
})

setMethod("PLS", signature = c(x = "SImageSet", y = "character"), 
	function(x, y, ...)
{
	PLS(x, factor(y), ...)
})

setMethod("predict", "PLS",
	function(object, newx, newy, ...)
{
	if ( !is(newx, "iSet") )
		.stop("'newx' must inherit from 'iSet'")
	.time.start()
	Xt <- as.matrix(iData(newx))
	Xt <- scale(t(Xt), scale=object$scale[[1]])
	if ( missing(newy) ) {
		missing.newy <- TRUE
	} else {
		missing.newy <- FALSE
	}
	out <- lapply(object@resultData, function(res) {
		.message("PLS: Predicting for ncomp = ", res$ncomp, ".")
		pred <- .PLS.predict(Xt, ncomp=res$ncomp,
			loadings=res$loadings, weights=res$weights,
			Yweights=res$Yweights)
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

.PLS.fit <- function(X, Y, ncomp, method, iter.max) {
	if ( method == "nipals" ) {
		nipals.PLS(X, Y, ncomp=ncomp, iter.max=iter.max)
	} else {
		stop("PLS method ", method, " not found")
	}
}

.PLS.predict <- function(X, ncomp, loadings, weights, Yweights) {
	loadings <- loadings[,1:ncomp,drop=FALSE]
	weights <- weights[,1:ncomp,drop=FALSE]
	Yweights <- Yweights[,1:ncomp,drop=FALSE]
	projection <- weights %*% solve(crossprod(loadings, weights))
	coefficients <- tcrossprod(projection, Yweights)
	scores <- X %*% projection
	fitted <- X %*% coefficients
	list(scores=scores, loadings=loadings, weights=weights, Yweights=Yweights,
		projection=projection, coefficients=coefficients, fitted=fitted)
}

