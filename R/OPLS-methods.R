
### implement methods for OPLS ####

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

