
### implement methods for PCA ####

setMethod("PCA", "MSImageSet", function(x, ncomp=2, standardize=FALSE,
	method=c("irlba", "svd", "nipals"), ...)
{
	x <- as.matrix(spectra(x))
	out <- PCA(x, ncomp=ncomp, isNxP=FALSE, standardize=standardize,
		method=method, ...)
	return(out)
} )

setMethod("PCA", "matrix", function(x, ncomp=2, isNxP=TRUE, standardize=FALSE,
	method=c("irlba", "svd", "nipals"), ...)
{
	method <- match.arg(method)
	if ( ncomp > ncol(x) ) stop("more components than variables")
	tryVerboseMessage("Centering data...", precedes.progress.output=FALSE)
	if ( isNxP ) {
		x <- scale(x, scale=standardize)
	} else {
		x <- scale(t(x), scale=standardize)
	}
	tryVerboseMessage("Projecting to principal components...", precedes.progress.output=FALSE)
	if ( method %in% c("irlba", "svd") ) {
		if ( method == "irlba" ) {
			svd <- irlba(x, nu=0, nv=ncomp, ...)
		} else {
			svd <- svd(x, nu=0, nv=ncomp, ...)
		}
		sdev <- svd$d[1:ncomp] / sqrt(max(1, nrow(x) - 1))
		loadings <- svd$v
		scores <- x %*% loadings
	} else if ( method == "nipals" ) {
		nipals <- nipals.PCA(x, ncomp=ncomp, ...)
		sdev <- nipals$sdev
		scores <- nipals$scores
		loadings <- nipals$loadings
	} else {
		stop("method ", method, " not recognized")
	}
	colnames(loadings) <- paste("PC", 1:ncomp, sep="")
	colnames(scores) <- paste("PC", 1:ncomp, sep="")
	outlist <- list(scores=scores, loadings=loadings, sdev=sdev, ncomp=ncomp,
		isNxP=isNxP, standardize=standardize, method=method)
	class(outlist) <- "PCA"
	attr(outlist, "scaled:center") <- attr(x, "scaled:center")
	attr(outlist, "scaled:scale") <- attr(x, "scaled:scale")
	return(outlist)
} )

