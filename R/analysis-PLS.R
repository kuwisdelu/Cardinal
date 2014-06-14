
### implement methods for PLS ####

setMethod("PLS", signature = c(x = "SImageSet", y = "matrix"), 
	function(x, y, ncomp = 20,
		method = "nipals",
		scale = FALSE, ...)
{
	# do stuff
} )

setMethod("PLS", signature = c(x = "SImageSet", y = "numeric"), 
	function(x, y,  ...)
{
	# do stuff
} )

setMethod("PLS", signature = c(x = "SImageSet", y = "character"), 
	function(x, y, ...)
{
	# do stuff
} )

setMethod("PLS", "MSImageSet", function(x, y, ncomp=2, standardize=FALSE,
	method="nipals", ...)
{
	x <- as.matrix(spectra(x))
	outlist <- PLS(x, y=y, ncomp=ncomp, isNxP=FALSE, standardize=standardize,
		method=method, ...)
	return(outlist)
} )

setMethod("PLS", "matrix", function(x, y, ncomp=2, isNxP=TRUE, standardize=FALSE,
	method="nipals", ...)
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
	tryVerboseMessage("Projecting to latent structures...", precedes.progress.output=FALSE)
	if ( method == "nipals" ) {
		fit <- nipals.PLS(x, y, ncomp=ncomp, ...)
	} else {
		stop("method ", method, " not recognized")
	}
	fit <- c(fit, list(ncomp=ncomp, isNxP=isNxP, standardize=standardize,
		method=method, metadata=list(isfactor=is.factor(predy))))
	if ( is.factor(predy) ) {
		fit$metadata$levels <- 1:ncol(y)
		fit$metadata$labels <- levels(predy)
	}
	class(fit) <- "PLS"
	attr(fit, "scaled:center") <- attr(x, "scaled:center")
	attr(fit, "scaled:scale") <- attr(x, "scaled:scale")
	attr(fit, "Yscaled:center") <- attr(y, "scaled:center")
	attr(fit, "Yscaled:scale") <- attr(y, "scaled:scale")
	outlist <- predict(fit, newx=predx, newy=predy)
	return(outlist)
} )

setMethod("predict", "PLS", function(object, newx, newy, ncomp=object$ncomp, ...) {
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
	scores <- x %*% object$projection[,1:ncomp,drop=FALSE]
	colnames(scores) <- paste("C", 1:ncomp, sep="")
	coefficients <- list()
	fitted <- list()
	for ( i in 1:ncomp ) {
		H <- object$weights[,1:i,drop=FALSE] %*% solve(crossprod(object$loadings[,1:i,drop=FALSE],
			object$weights[,1:i,drop=FALSE]))
		coefficients[[i]] <- tcrossprod(H, object$Yweights[,1:i,drop=FALSE])
		fitted[[i]] <- x %*% coefficients[[i]]
		if ( object$standardize ) {
			Yscale <- attr(object, "Yscaled:scale")
		} else {
			Yscale <- 1
		}
		fitted[[i]] <- Yscale * fitted[[i]] + attr(object, "Yscaled:center")
	}
	names(coefficients) <- paste(1:ncomp, "components")
	names(fitted) <- paste(1:ncomp, "components")
	outlist <- object
	outlist$scores <- scores
	outlist$coefficients <- coefficients
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
