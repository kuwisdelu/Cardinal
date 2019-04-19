
setMethod("PCA", "SparseImagingExperiment", 
	function(x, ncomp = 3, center = TRUE, scale = FALSE, ...)
	{
		.checkForIncompleteProcessing(x)
		if ( max(ncomp) > ncol(x) )
			.stop("can't fit more components than number of pixels")
		if ( max(ncomp) > nrow(x) )
			.stop("can't fit more components than number of features")
		if ( length(ncomp) > 1L )
			ncomp <- max(ncomp)
		.message("projecting ", ncomp, " principal components...")
		results <- lapply(ncomp, function(nc) {
			.PCA2_fit(x, ncomp=nc, center=center, scale=scale)
		})
		models <- DataFrame(ncomp=ncomp)
		.PCA2(
			imageData=.SimpleImageArrayList(),
			featureData=featureData(x),
			elementMetadata=pixelData(x),
			metadata=list(
				mapping=list(
					feature="loadings",
					pixel="scores"),
				parameters=names(models)),
			resultData=as(results, "List"),
			modelData=models)
	})

setMethod("predict", "PCA2",
	function(object, newdata, ncomp, ...)
	{
		if ( !is(newdata, "SparseImagingExperiment") )
			.stop("'newdata' must inherit from 'SparseImagingExperiment'")
		.checkForIncompleteProcessing(newdata)
		if ( missing(ncomp) )
			ncomp <- max(modelData(object)$ncomp)
		.message("projecting ", ncomp, " principal components...")
		results <- lapply(resultData(object), function(res) {
			.PCA2_predict(newdata, ncomp=ncomp, loadings=res$loadings,
				center=res$center, scale=res$scale)
		})
		models <- DataFrame(ncomp=ncomp)
		.PCA2(
			imageData=.SimpleImageArrayList(),
			featureData=featureData(newdata),
			elementMetadata=pixelData(newdata),
			metadata=list(
				mapping=list(
					feature="loadings",
					pixel="scores"),
				parameters=names(models)),
			resultData=as(results, "List"),
			modelData=models)
	})

.PCA2_fit <- function(x, ncomp, center, scale, ...) {
	if ( is(iData(x), "matter_mat") ) {
		Xt <- t(iData(x))
	} else {
		Xt <- t(as.matrix(iData(x)))
	}
	Xt <- scale(Xt, center=center, scale=scale)
	if ( isTRUE(center) )
		center <- attr(Xt, "scaled:center")
	if ( isTRUE(scale) )
		scale <- attr(Xt, "scaled:scale")
	sv <- irlba(Xt, nu=ncomp, nv=ncomp, fastpath=is.matrix(Xt))
	sdev <- sv$d[1:ncomp] / sqrt(max(1, nrow(Xt) - 1))
	loadings <- sv$v
	scores <- Xt %*% loadings
	colnames(loadings) <- paste("PC", 1:ncomp, sep="")
	colnames(scores) <- paste("PC", 1:ncomp, sep="")
	list(scores=scores, loadings=loadings, sdev=sdev,
		center=center, scale=scale)
}

.PCA2_predict <- function(x, ncomp, loadings, center, scale, ...) {
	if ( is(iData(x), "matter_mat") ) {
		Xt <- t(iData(x))
	} else {
		Xt <- t(as.matrix(iData(x)))
	}
	Xt <- scale(Xt, center=center, scale=scale)
	scores <- Xt %*% loadings[,1:ncomp,drop=FALSE]
	sdev <- apply(scores, 2, sd)
	colnames(scores) <- paste("PC", 1:ncomp, sep="")
	list(scores=scores, loadings=loadings, sdev=sdev,
		center=center, scale=scale)
}

