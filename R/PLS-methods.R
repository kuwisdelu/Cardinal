
### implement methods for PLS ####

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

