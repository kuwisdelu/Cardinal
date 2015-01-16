
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
		nas <- apply(y, 1, function(yi) any(is.na(yi)))
		newx <- x
		newy <- y
		if ( any(nas) ) {
			.message("PLS: Removing missing observations.")
			x <- x[,!nas]
			y <- y[!nas,]
		}
		.time.start()
		.message("PLS: Centering data.")
		Xt <- as.matrix(iData(x))
		Xt <- scale(t(Xt), scale=scale)
		Y <- scale(y, scale=scale)
		if ( scale ) {
			scale <- attr(Xt, "scaled:scale")
			Yscale <- attr(Y, "scaled:scale")
		} else {
			scale <- rep(1, ncol(Xt))
			names(scale) <- colnames(Xt)
			Yscale <- rep(1, ncol(Y))
			names(Yscale) <- colnames(Y)
		}
		center <- attr(Xt, "scaled:center")
		Ycenter <- attr(Y, "scaled:center")
		.message("PLS: Fitting partial least squares components.")
		fit <- .PLS.fit(Xt, Y, ncomp=max(ncomps), method=method, iter.max=iter.max)
		result <- lapply(ncomps, function(ncomp) {
			append(fit, list(y=newy, ncomp=ncomp,
				method=method, scale=scale, center=center,
				Yscale=Yscale, Ycenter=Ycenter))
		})
		model <- AnnotatedDataFrame(
			data=data.frame(ncomp=sapply(result, function(fit) fit$ncomp)),
			varMetadata=data.frame(
				labelDescription=c(ncomp="Number of PLS Components")))
		featureNames(model) <- .format.data.frame(pData(model))
		names(result) <- .format.data.frame(pData(model))
		object <- new("PLS",
			pixelData=x@pixelData,
			featureData=x@featureData,
			experimentData=x@experimentData,
			protocolData=x@protocolData,
			resultData=result,
			modelData=model)
		.time.stop()
		predict(object, newx=newx, newy=newy)
	})

setMethod("PLS", signature = c(x = "SImageSet", y = "numeric"), 
	function(x, y,  ...)
	{
		PLS(x, as.matrix(y), ...)
	})

setMethod("PLS", signature = c(x = "SImageSet", y = "factor"), 
	function(x, y, ...)
	{
		newy <- sapply(levels(y), function(Ck) as.integer(y == Ck))
		attr(newy, "PLS:y") <- y
		PLS(x, newy, ...)
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
		Xt <- scale(t(Xt), center=object$center[[1]],
			scale=object$scale[[1]])
		Y <- object$y[[1]]
		if ( missing(newy) ) {
			missing.newy <- TRUE
		} else {
			missing.newy <- FALSE
		}
		result <- lapply(object@resultData, function(res) {
			.message("PLS: Predicting for ncomp = ", res$ncomp, ".")
			pred <- .PLS.predict(Xt, Y, ncomp=res$ncomp,
				loadings=res$loadings, weights=res$weights,
				Yweights=res$Yweights)
			pred$fitted <- t(res$Yscale * t(pred$fitted) + res$Ycenter)
			if ( is.factor(object$y) || !missing.newy ) {
				pred$classes <- factor(apply(pred$fitted, 1, which.max))
				if ( !is.null(attr(newy, "PLS:y")) ) {
					newy <- attr(newy, "PLS:y")
					levels(pred$classes) <- levels(newy)
				} else {
					levels(pred$classes) <- levels(object$y[[1]])
				}	
			}
			res[names(pred)] <- pred
			if ( !missing.newy )
				res$y <- newy
			res
		})
		.message("PLS: Done.")
		.time.stop()
		new("PLS",
			pixelData=newx@pixelData,
			featureData=newx@featureData,
			experimentData=newx@experimentData,
			protocolData=newx@protocolData,
			resultData=result,
			modelData=object@modelData)
	})

.PLS.fit <- function(X, Y, ncomp, method, iter.max) {
	if ( method == "nipals" ) {
		nipals.PLS(X, Y, ncomp=ncomp, iter.max=iter.max)
	} else {
		stop("PLS method ", method, " not found")
	}
}

.PLS.predict <- function(X, Y, ncomp, loadings, weights, Yweights) {
	loadings <- loadings[,1:ncomp,drop=FALSE]
	weights <- weights[,1:ncomp,drop=FALSE]
	Yweights <- Yweights[,1:ncomp,drop=FALSE]
	projection <- weights %*% solve(crossprod(loadings, weights))
	coefficients <- tcrossprod(projection, Yweights)
	scores <- X %*% projection
	fitted <- X %*% coefficients
	if ( is.factor(Y) ) {
		rownames(Yweights) <- levels(Y)
		colnames(coefficients) <- levels(Y)
		colnames(fitted) <- levels(Y)
	} else {
		rownames(Yweights) <- colnames(Y)
		colnames(coefficients) <- colnames(Y)
		colnames(fitted) <- colnames(Y)
	}
	list(scores=scores, loadings=loadings, weights=weights, Yweights=Yweights,
		projection=projection, coefficients=coefficients, fitted=fitted)
}

