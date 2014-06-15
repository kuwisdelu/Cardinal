
### implement methods for PCA ####

setMethod("PCA", signature = c(x = "SImageSet"), 
	function(x, ncomp = 20,
		method = c("irlba", "svd"),
		scale = FALSE, ...)
{
	method <- match.arg(method)
	.time.start()
	.message("PCA: Fitting principal components.")
	ncomps <- sort(ncomp)
	fit <- .PCA.fit(x, ncomp=max(ncomps), method=method, scale=scale)
	out <- lapply(ncomps, function(ncomp) {
		loadings <- fit$loadings[,1:ncomp,drop=FALSE]
		scores <- fit$scores[,1:ncomp,drop=FALSE]
		sdev <- fit$sdev[1:ncomp]
		res <- list(scores=scores, loadings=loadings, sdev=sdev,
			method=method, ncomp=ncomp,
			center=fit$center, scale=fit$scale)
		class(res) <- "ResultData"
		res
	})
	par <- AnnotatedDataFrame(
		data=data.frame(ncomp=sapply(out, function(fit) fit$ncomp)),
		varMetadata=data.frame(
			labelDescription=c(ncomp="Number of Principal Components")))
	featureNames(par) <- formatParam(pData(par))
	names(out) <- formatParam(pData(par))
	.message("PCA: Done.")
	.time.stop()
	new("PCA",
		pixelData=x@pixelData,
		featureData=x@featureData,
		experimentData=x@experimentData,
		protocolData=x@protocolData,
		resultData=out,
		modelData=par)
})

setMethod("predict", "PCA",
	function(object, newx, ...)
{
	.time.start()
	.message("PCA: Predicting principal components scores.")
	out <- lapply(object@resultData, function(res) {
		.message("PCA: Predicting scores for ncomp = ", res$ncomp, ".")
		pred <- .PCA.predict(newx, ncomp=res$ncomp, loadings=res$loadings,
			center=res$center, scale=res$scale)
		res[names(pred)] <- pred
		class(res) <- "ResultData"
		res
	})
	.message("PCA: Done.")
	.time.stop()
	new("PCA",
		pixelData=x@pixelData,
		featureData=x@featureData,
		experimentData=x@experimentData,
		protocolData=x@protocolData,
		resultData=out,
		modelData=par)
})

.PCA.fit <- function(x, ncomp, method, scale) {
	Xt <- as.matrix(iData(x))
	Xt <- scale(t(Xt), scale=scale)
	if ( scale )
		scale <- attr(Xt, "scaled:scale")
	center <- attr(Xt, "scaled:center")
	if ( method == "irlba" ) {
		svd <- irlba(Xt, nu=0, nv=ncomp)
	} else {
		svd <- svd(Xt, nu=0, nv=ncomp)
	}
	sdev <- svd$d[1:ncomp] / sqrt(max(1, nrow(Xt) - 1))
	loadings <- svd$v
	scores <- Xt %*% loadings
	rownames(loadings) <- featureNames(x)
	colnames(loadings) <- paste("PC", 1:ncomp, sep="")
	rownames(scores) <- pixelNames(x)
	colnames(scores) <- paste("PC", 1:ncomp, sep="")
	list(scores=scores, loadings=loadings, sdev=sdev,
		method=method, center=center, scale=scale)
}

.PCA.predict <- function(x, ncomp, loadings, center, scale) {
	Xt <- as.matrix(iData(x))
	Xt <- scale(t(Xt), scale=scale)
	scores <- Xt %*% loadings[,1:ncomp,drop=FALSE]
	sdev <- apply(scores, 2, sd)
	rownames(scores) <- pixelNames(x)
	colnames(scores) <- paste("PC", 1:ncomp, sep="")
	list(scores=scores, loadings=loadings, sdev=sdev)
}

