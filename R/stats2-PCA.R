
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
		results <- .PCA2_fit(Xt, ncomp=ncomp)
		scaled <- SimpleList(center=center, scale=scale)
		models <- DataFrame(ncomp=ncomp)
		.PCA2(
			imageData=.SimpleImageArrayList(),
			featureData=featureData(x),
			elementMetadata=pixelData(x),
			metadata=list(
				mapping=metadata(x)$mapping,
				parameters=names(models),
				scaled=scaled),
			resultData=as(list(results), "List"),
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
		if ( is(iData(newdata), "matter_mat") ) {
			Xt <- t(iData(newdata))
		} else {
			Xt <- t(as.matrix(iData(newdata)))
		}
		scaled <- metadata(object)$scaled
		Xt <- scale(Xt, center=scaled$center, scale=scaled$scale)
		results <- mapply(function(res, nc) {
			.PCA2_predict(Xt, ncomp=nc, loadings=res$loadings)
		}, resultData(object), ncomp, SIMPLIFY=FALSE)
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

.PCA2_fit <- function(x, ncomp, ...) {
	sv <- irlba(x, nu=ncomp, nv=ncomp, fastpath=is.matrix(x))
	sdev <- sv$d[1:ncomp] / sqrt(max(1, nrow(x) - 1))
	loadings <- sv$v
	scores <- x %*% loadings
	colnames(loadings) <- paste("PC", 1:ncomp, sep="")
	colnames(scores) <- paste("PC", 1:ncomp, sep="")
	list(scores=scores, loadings=loadings, sdev=sdev)
}

.PCA2_predict <- function(x, ncomp, loadings, ...) {
	scores <- x %*% loadings[,1:ncomp,drop=FALSE]
	sdev <- apply(scores, 2, sd)
	colnames(scores) <- paste("PC", 1:ncomp, sep="")
	list(scores=scores, loadings=loadings, sdev=sdev)
}

