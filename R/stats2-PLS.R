
setMethod("PLS",
	signature = c(x = "SparseImagingExperiment", y = "ANY"),
	function(x, y, ncomp = 3, method = c("pls", "opls"),
		center = TRUE, scale = FALSE,
		iter.max = 100, ...)
	{
		.checkForIncompleteProcessing(x)
		method <- match.arg(method)
		if ( max(ncomp) > ncol(x) )
			.stop("can't fit more components than number of pixels")
		if ( max(ncomp) > nrow(x) )
			.stop("can't fit more components than number of features")
		if ( length(ncomp) > 1L ) {
			nc <- max(ncomp)
		} else {
			nc <- ncomp
		}
		.message("preparing data...")
		if ( is.factor(y) || is.character(y) ) {
			type <- "classification"
			Y <- .factor_matrix(y)
		} else {
			type <- "regression"
			Y <- as.matrix(y)
		}
		Xt <- t(as.matrix(iData(x)))
		Xt <- scale(Xt, center=center, scale=scale)
		Y <- scale(Y, center=center, scale=scale)
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
		}
		.message("projecting ", nc, " latent components...")
		results <- .PLS2_fit(Xt, Y, ncomp=nc,
			method=method, iter.max=iter.max)
		scaled <- SimpleList(center=center, scale=scale,
			Ycenter=Ycenter, Yscaled=Yscale)
		models <- DataFrame(ncomp=nc)
		out <- .PLS2(
			imageData=.SimpleImageArrayList(),
			featureData=featureData(x),
			elementMetadata=pixelData(x),
			metadata=list(
				mapping=list(
					feature=c("coefficients", "loadings", "weights"),
					pixel=c("fitted", "scores")),
				method=method, scaled=scaled,
				type=type),
			resultData=as(list(results), "List"),
			modelData=models)
		if ( method == "opls" )
			out <- as(out, "OPLS2")
		predict(out, newx=x, newy=y, ncomp=ncomp)
	})

setMethod("OPLS",
	signature = c(x = "SparseImagingExperiment", y = "ANY"),
	function(x, y, ncomp = 3, ...)
	{
		PLS(x, y, ncomp=ncomp, method="opls", ...)
	})

setMethod("predict", "PLS2",
	function(object, newx, newy, ncomp, ...)
	{
		if ( !is(newx, "SparseImagingExperiment") )
			.stop("'newx' must inherit from 'SparseImagingExperiment'")
		.checkForIncompleteProcessing(newx)
		if ( missing(ncomp) )
			ncomp <- modelData(object)$ncomp
		type <- metadata(object)$type
		.message("preparing data...")
		Xt <- t(as.matrix(iData(newx)))
		scaled <- metadata(object)$scaled
		method <- metadata(object)$method
		Xt <- scale(Xt, center=scaled$center, scale=scaled$scale)
		result_t <- resultData(object)[which.max(modelData(object)$ncomp)]
		results <- mapply(function(res, nc) {
			.message("predicting using ", nc, " latent components...")
			res <- .PLS2_predict(Xt, ncomp=nc, method=method,
				loadings=res$loadings, weights=res$loadings,
				Yweights=res$Yweights, Oloadings=res$Oloadings,
				Oweights=res$Oweights)
			if ( isFALSE(scaled$Ycenter) ) {
				Ycenter <- 0
			} else {
				Ycenter <- scaled$Ycenter
			}
			if ( isFALSE(scaled$Yscale) ) {
				Yscale <- 1
			} else {
				Yscale <- scaled$Yscale
			}
			res$fitted <- t(Yscale * t(res$fitted) + Ycenter)
			if ( metadata(object)$type == "classification" ) {
				class <- apply(res$fitted, 1, which.max)
				res$class <- factor(class,
					levels=seq_len(ncol(res$fitted)),
					labels=colnames(res$fitted))
			}
			res
		}, result_t, ncomp, SIMPLIFY=FALSE)
		models <- DataFrame(ncomp=ncomp)
		out <- .PLS2(
			imageData=.SimpleImageArrayList(),
			featureData=featureData(newx),
			elementMetadata=pixelData(newx),
			metadata=metadata(object),
			resultData=as(results, "List"),
			modelData=models)
		mapping <- metadata(object)$mapping
		if ( type == "classification" )
			mapping$pixel <- c(mapping$pixel, "class")
		if ( method == "opls" )
			mapping$feature <- c(mapping$pixel, c("Oloadings", "Oweights"))
		metadata(out)$mapping <- mapping
		if ( method == "opls" )
			out <- as(out, "OPLS2")
		if ( !missing(newy) ) {
			if ( is.factor(newy) || is.character(newy) ) {
				pixelData(out)$.response <- as.factor(newy)
			} else {
				newy <- as.matrix(newy)
				i <- if (ncol(newy) > 1) seq_len(ncol(newy)) else ""
				nms <- paste0(".response", i)
				pixelData(out)[nms] <- as.data.frame(newy)
			}
		}
		out
	})

setMethod("fitted", "PLS2",
	function(object, ...) {
		if ( metadata(object)$type == "classification" ) {
			object$class
		} else {
			object$fitted
		}
	})

setAs("PLS", "PLS2",
	function(from) {
		to <- .coerce_ImagingResult(from, "PLS2")
		metadata(to)$mapping <- list(
					feature=c("coefficients", "loadings", "weights"),
					pixel=c("fitted", "scores"))
		if ( !is.null(resultData(to, 1, "y")) ) {
			y <- resultData(to, 1, "y")
			if ( is.factor(y) || is.character(y) ) {
				resultData(to) <- endoapply(resultData(to),
					function(res) rename(res, classes="class"))
				metadata(to)$type <- "classification"
			} else {
				metadata(to)$type <- "regression"
			}
			metadata(to)$method <- "pls"
			if ( is.null(dim(y)) )
				pixelData(to)$.response <- y
		}
		to
	})

setAs("OPLS", "OPLS2",
	function(from) {
		to <- .coerce_ImagingResult(from, "OPLS2")
		metadata(to)$mapping <- list(
					feature=c("coefficients", "loadings", "weights"),
					pixel=c("fitted", "scores"))
		if ( !is.null(resultData(to, 1, "y")) ) {
			y <- resultData(to, 1, "y")
			if ( is.factor(y) || is.character(y) ) {
				resultData(to) <- endoapply(resultData(to),
					function(res) rename(res, classes="class"))
				metadata(to)$type <- "classification"
			} else {
				metadata(to)$type <- "regression"
			}
			metadata(to)$method <- "opls"
			if ( is.null(dim(y)) )
				pixelData(to)$.response <- y
		}
		to
	})

.PLS2_fit <- function(X, Y, ncomp, method, iter.max) {
	nas <- apply(Y, 1, anyNA)
	X <- X[!nas,,drop=FALSE]
	Y <- Y[!nas,,drop=FALSE]
	if ( method == "opls" ) {
		fit <- nipals.OPLS(X, Y, ncomp=ncomp, iter.max=iter.max)
		fit$loadings <- matrix(nrow=ncol(X), ncol=ncomp,
			dimnames=dimnames(fit$Oloadings))
		fit$weights <- matrix(nrow=ncol(X), ncol=ncomp,
			dimnames=dimnames(fit$Oweights))
		fit$Yweights <- matrix(nrow=ncol(Y), ncol=ncomp,
			dimnames=list(colnames(Y), colnames(fit$Oweights)))
		for ( nc in 1:ncomp ) {
			Oloadings <- fit$Oloadings[,1:nc,drop=FALSE]
			Oweights <- fit$Oweights[,1:nc,drop=FALSE]
			Oscores <- X %*% Oweights
			Xortho <- tcrossprod(Oscores, Oloadings)
			Xnew <- X - Xortho
			fit1 <- nipals.PLS(Xnew, Y, ncomp=1, iter.max=iter.max)
			fit$loadings[,nc] <- fit1$loadings[,1]
			fit$weights[,nc] <- fit1$weights[,1]
			fit$Yweights[,nc] <- fit1$Yweights[,1]
		}
	} else {
		fit <- nipals.PLS(X, Y, ncomp=ncomp, iter.max=iter.max)
	}
	fit
}

.PLS2_predict <- function(X, ncomp, method, loadings, weights,
	Yweights, Oloadings, Oweights)
{
	if ( method == "opls" ) {
		Oloadings <- Oloadings[,1:ncomp,drop=FALSE]
		Oweights <- Oweights[,1:ncomp,drop=FALSE]
		Oscores <- X %*% Oweights
		Xortho <- tcrossprod(Oscores, Oloadings)
		X <- X - Xortho
		loadings_t <- loadings[,ncomp,drop=FALSE]
		weights_t <- weights[,ncomp,drop=FALSE]
		Yweights_t <- Yweights[,ncomp,drop=FALSE]
		opls_out <- list(Oloadings=Oloadings, Oweights=Oweights)
	} else {
		loadings_t <- loadings[,1:ncomp,drop=FALSE]
		weights_t <- weights[,1:ncomp,drop=FALSE]
		Yweights_t <- Yweights[,1:ncomp,drop=FALSE]
		opls_out <- list()
	}
	projection <- weights_t %*% solve(crossprod(loadings_t, weights_t))
	coefficients <- tcrossprod(projection, Yweights_t)
	scores <- X %*% projection
	fitted <- X %*% coefficients
	colnames(coefficients) <- rownames(Yweights)
	colnames(fitted) <- rownames(Yweights)
	pls_out <- list(fitted=fitted, scores=scores,
		loadings=loadings[,1:ncomp,drop=FALSE],
		weights=weights[,1:ncomp,drop=FALSE],
		Yweights=Yweights[,1:ncomp,drop=FALSE],
		projection=projection, coefficients=coefficients)
	c(pls_out, opls_out)
}

