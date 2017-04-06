
### implement methods for OPLS ####

setMethod("OPLS", signature = c(x = "SImageSet", y = "matrix"), 
	function(x, y, ncomp = 20,
		method = "nipals",
		center = TRUE,
		scale = FALSE,
		keep.Xnew = TRUE,
		iter.max = 100, ...)
	{
		method <- match.arg(method)
		ncomps <- sort(ncomp)
		if ( max(ncomps) > nrow(x) )
			.stop("OPLS: Can't fit more components than extent of dataset")
		nas <- apply(y, 1, function(yi) any(is.na(yi)))
		newx <- x
		newy <- y
		if ( any(nas) ) {
			.message("OPLS: Removing missing observations.")
			x <- x[,!nas]
			y <- y[!nas,]
		}
		.time.start()
		.message("OPLS: Centering data.")
		Xt <- t(as.matrixlike(iData(x), supported="matrix"))
		Xt <- scale(Xt, center=center, scale=scale)
		Y <- scale(y, center=center, scale=scale)
		if ( center ) {
			center <- attr(Xt, "scaled:center")
			Ycenter <- attr(Y, "scaled:center")
		} else {
			Ycenter <- FALSE
		}
		if ( scale ) {
			scale <- attr(Xt, "scaled:scale")
			Yscale <- attr(Y, "scaled:scale")
		} else {
			Yscale <- FALSE
			scale <- rep(1, ncol(Xt))
			names(scale) <- colnames(Xt)
			Yscale <- rep(1, ncol(Y))
			names(Yscale) <- colnames(Y)
		}
		.message("OPLS: Fitting orthogonal partial least squares components.")
		fit <- .OPLS.fit(Xt, Y, ncomp=max(ncomps), method=method, iter.max=iter.max)
		result <- lapply(ncomps, function(ncomp) {
			append(fit, list(y=newy, ncomp=ncomp,
				method=method, center=center, scale=scale,
				Ycenter=Ycenter, Yscale=Yscale))
		})
		model <- AnnotatedDataFrame(
			data=data.frame(ncomp=sapply(result, function(fit) fit$ncomp)),
			varMetadata=data.frame(
				labelDescription=c(ncomp="Number of PLS Components")))
		featureNames(model) <- .format.data.frame(pData(model))
		names(result) <- .format.data.frame(pData(model))
		object <- new("OPLS",
			pixelData=x@pixelData,
			featureData=x@featureData,
			experimentData=x@experimentData,
			protocolData=x@protocolData,
			resultData=result,
			modelData=model)
		.time.stop()
		predict(object, newx=newx, newy=newy, keep.Xnew=keep.Xnew, ...)
	})

setMethod("OPLS", signature = c(x = "SImageSet", y = "numeric"), 
	function(x, y,  ...)
	{
		OPLS(x, as.matrix(y), ...)
	})

setMethod("OPLS", signature = c(x = "SImageSet", y = "factor"), 
	function(x, y, ...)
	{
		newy <- sapply(levels(y), function(Ck) as.integer(y == Ck))
		attr(newy, "OPLS:y") <- y
		OPLS(x, newy, ...)
	})

setMethod("OPLS", signature = c(x = "SImageSet", y = "character"), 
	function(x, y, ...)
	{
		OPLS(x, factor(y), ...)
	})

setMethod("predict", "OPLS",
	function(object, newx, newy, keep.Xnew = TRUE, ...)
	{
		if ( !is(newx, "iSet") )
			.stop("'newx' must inherit from 'iSet'")
		.time.start()
		Xt <- t(as.matrixlike(iData(newx), supported="matrix"))
		Xt <- scale(Xt, center=object$center[[1]], scale=object$scale[[1]])
		Y <- object$y[[1]]
		if ( missing(newy) ) {
			missing.newy <- TRUE
		} else {
			missing.newy <- FALSE
		}
		result <- lapply(object@resultData, function(res) {
			.message("OPLS: Predicting for ncomp = ", res$ncomp, ".")
			pred <- .OPLS.predict(Xt, Y, ncomp=res$ncomp, method=res$method,
				loadings=res$loadings, Oloadings=res$Oloadings,
				weights=res$weights, Oweights=res$Oweights,
				Yweights=res$Yweights)
			if ( is.logical(res$Ycenter) && !Ycenter ) {
				Ycenter <- 0
			} else {
				Ycenter <- res$Ycenter
			}
			if ( is.logical(res$Yscale) && !Yscale ) {
				Yscale <- 1
			} else {
				Yscale <- res$Yscale
			}
			pred$fitted <- t(Yscale * t(pred$fitted) + Ycenter)
			if ( is.factor(object$y) || !missing.newy ) {
				pred$classes <- factor(apply(pred$fitted, 1, which.max))
				if ( !is.null(attr(newy, "OPLS:y")) ) {
					newy <- attr(newy, "OPLS:y")
					levels(pred$classes) <- levels(newy)
				} else {
					levels(pred$classes) <- levels(object$y[[1]])
				}	
			}
			res[names(pred)] <- pred
			if ( !keep.Xnew ) {
				res$Xnew <- NULL
				res$Xortho <- NULL
			}
			if ( !missing.newy )
				res$y <- newy
			res
		})
		.message("OPLS: Done.")
		.time.stop()
		new("OPLS",
			pixelData=newx@pixelData,
			featureData=newx@featureData,
			experimentData=newx@experimentData,
			protocolData=newx@protocolData,
			resultData=result,
			modelData=object@modelData)
	})

.OPLS.fit <- function(X, Y, ncomp, method, iter.max) {
	if ( method == "nipals" ) {
		nipals.OPLS(X, Y, ncomp=ncomp, iter.max=iter.max)
	} else {
		stop("OPLS method ", method, " not found")
	}
}

.OPLS.predict <- function(X, Y, ncomp, method, loadings, Oloadings,
	weights, Oweights, Yweights, iter.max)
{
	Oloadings <- Oloadings[,1:ncomp,drop=FALSE]
	Oweights <- Oweights[,1:ncomp,drop=FALSE]
	Oscores <- X %*% Oweights
	Xortho <- tcrossprod(Oscores, Oloadings)
	Xnew <- X - Xortho
	if ( is.null(loadings) || is.null(weights) || is.null(Yweights) ) {
		nas <- apply(Y, 1, function(yi) any(is.na(yi)))
		if ( method == "nipals" ) {
			fit <- nipals.PLS(Xnew[!nas,], Y[!nas,], ncomp=1)
		} else {
			stop("PLS method ", method, " not found")
		}
	} else {
		fit <- list(loadings=loadings, weights=weights, Yweights=Yweights)
	}
	pred <- .PLS.predict(Xnew, Y, ncomp=1, loadings=fit$loadings,
		weights=fit$weights, Yweights=fit$Yweights)
	append(pred, list(Xnew=Xnew, Xortho=Xortho, Oscores=Oscores,
		Oloadings=Oloadings, Oweights=Oweights))
}

