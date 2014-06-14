
### implement methods for OPLS ####

setMethod("OPLS", signature = c(x = "SImageSet", y = "matrix"), 
	function(x, y, ncomp = 20,
		method = "nipals",
		scale = FALSE, ...)
{
	# do stuff
} )

setMethod("OPLS", signature = c(x = "SImageSet", y = "numeric"), 
	function(x, y, ...)
{
	# do stuff
} )

setMethod("OPLS", signature = c(x = "SImageSet", y = "character"), 
	function(x, y, ...)
{
	# do stuff
} )

setMethod("OPLS", "MSImageSet", function(x, y, ncomp=1, acomp=1, standardize=FALSE,
	method="nipals", ...)
{
	x <- as.matrix(spectra(x))
	outlist <- OPLS(x, y=y, ncomp=ncomp, acomp=acomp, isNxP=FALSE,
		standardize=standardize, method=method, ...)
	return(outlist)
} )

setMethod("OPLS", "matrix", function(x, y, ncomp=1, acomp=1, isNxP=TRUE,
	standardize=FALSE, method="nipals", ...)
{
	predx <- x
	predy <- y
	method <- match.arg(method)
	if ( is.factor(y) ) y <- calculateClassMasks(y)
	if ( !is.matrix(y) ) y <- as.matrix(y)
	nonmissing <- apply(y, 1, function(yi) all(is.finite(yi)))
	y <- y[nonmissing,,drop=FALSE]
	y <- scale(y, scale=standardize)
	tryVerboseMessage("Centering data...", precedes.progress.output=FALSE)
	if ( isNxP ) {
		x <- scale(x[nonmissing,], scale=standardize)
	} else {
		x <- scale(t(x[,nonmissing]), scale=standardize)
	}
	if ( ncomp > ncol(x) ) stop("more components than variables")
	tryVerboseMessage("Removing orthogonal variation...", precedes.progress.output=FALSE)
	if ( method == "nipals" ) {
		fit <- nipals.OPLS(x, y, ncomp=ncomp, ...)
	} else {
		stop("method ", method, " not recognized")
	}
	coefficients <- list()
	weights <- list()
	loadings <- list()
	scores <- list()
	tryVerboseMessage("Performing PLS on processed data...", precedes.progress.output=FALSE)
	for ( i in 1:ncomp) {
		x <- x - tcrossprod(fit$Oscores[,i], fit$Oloadings[,i])
		temp <- PLS(x, y, ncomp=acomp, method=method)
		coefficients[[i]] <- temp$coefficients[[acomp]]
		weights[[i]] <- temp$weights
		loadings[[i]] <- temp$loadings
		scores[[i]] <- temp$scores
	}
	names(coefficients) <- paste(1:ncomp, "+", acomp, "components")
	fit <- c(fit, list(coefficients=coefficients, weights=weights, loadings=loadings,
		scores=scores, ncomp=ncomp, acomp=acomp, isNxP=isNxP, standardize=standardize,
		method=method, metadata=list(isfactor=is.factor(predy))))
	if ( is.factor(predy) ) {
		fit$metadata$levels <- 1:ncol(y)
		fit$metadata$labels <- levels(predy)
	}
	class(fit) <- "OPLS"
	attr(fit, "scaled:center") <- attr(x, "scaled:center")
	attr(fit, "scaled:scale") <- attr(x, "scaled:scale")
	attr(fit, "Yscaled:center") <- attr(y, "scaled:center")
	attr(fit, "Yscaled:scale") <- attr(y, "scaled:scale")
	outlist <- predict(fit, newx=predx, newy=predy)
	return(outlist)
} )

setMethod("predict", "OPLS", function(object, newx, newy, ncomp=object$ncomp, ...) {
	if ( isMSImageSet(newx) ) newx <- as.matrix(spectra(newx))
	tryVerboseMessage("Predicting...", precedes.progress.output=FALSE)
	if ( ncomp > object$ncomp ) stop("too many components")
	if ( object$standardize ) {
		standardize <- attr(object, "scaled:scale")
	} else {
		standardize <- FALSE
	}
	tryVerboseMessage("Centering data...", precedes.progress.output=FALSE)
	if ( object$isNxP ) {
		x <- scale(newx, center=attr(object, "scaled:center"), scale=standardize)
	} else {
		x <- scale(t(newx), center=attr(object, "scaled:center"), scale=standardize)
	}
	tryVerboseMessage("Calculating new scores and fitted values...", precedes.progress.output=FALSE)
	scores <- matrix(nrow=nrow(x), ncol=ncomp)
	fitted <- list()
	for ( i in 1:ncomp) {
		scores[,i] <- x %*% object$Oweights[,i]
		x <- x - tcrossprod(scores[,i], object$Oloadings[,i])
		fitted[[i]] <- x %*% object$coefficients[[i]]
		if ( object$standardize ) {
			Yscale <- attr(object, "Yscaled:scale")
		} else {
			Yscale <- 1
		}
		fitted[[i]] <- Yscale * fitted[[i]] + attr(object, "Yscaled:center")
	}
	names(fitted) <- paste(1:ncomp, "+", object$acomp, "components")
	outlist <- object
	outlist$Xnew <- x
	outlist$Oscores <- scores
	outlist$fitted <- fitted
	if ( outlist$metadata$isfactor ) {
		outlist$classes <- list()
		for ( i in seq_along(outlist$fitted) ) {
			temp <- apply(outlist$fitted[[i]], 1, which.max)
			outlist$classes[[i]] <- factor(temp,
				levels=outlist$metadata$levels,
				labels=outlist$metadata$labels)
		}
	}
	if ( !missing(newy) ) {
		tryVerboseMessage("Calculating residuals...", precedes.progress.output=FALSE)
		if ( outlist$metadata$isfactor ) {
			outlist$misclassified <- lapply(outlist$classes, function(yhat) newy != yhat)
			error <- outlist$misclassified
			names(outlist$misclassified) <- paste(1:ncomp, "components")
		} else {
			outlist$residuals <- lapply(outlist$fitted, function(yhat) newy - yhat)
			error <- outlist$residuals
			names(outlist$residuals) <- paste(1:ncomp, "components")
		}
		outlist$MSEP <- sapply(error, function(e) sum(e^2,
			na.rm=TRUE) / sum(is.finite(as.numeric(e))))
		names(outlist$MSEP) <- paste(1:ncomp, "components")
	}
	return(outlist)
} )

