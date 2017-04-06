
### implement methods for PCA ####

setMethod("PCA", signature = c(x = "SImageSet"), 
	function(x, ncomp = 20,
		method = c("irlba", "nipals", "svd"),
		center = TRUE,
		scale = FALSE,
		iter.max = 100, ...)
	{
		method <- match.arg(method)
		ncomps <- sort(ncomp)
		if ( max(ncomps) > nrow(x) )
			.stop("PCA: Can't fit more components than extent of dataset")
		.time.start()
		.message("PCA: Fitting principal components.")
		if ( max(ncomps) > nrow(x) )
			.stop("PCA: Can't fit more components than extent of dataset")
		fit <- .PCA.fit(x, ncomp=max(ncomps), method=method,
			center=center, scale=scale, iter.max=iter.max)
		result <- lapply(ncomps, function(ncomp) {
			loadings <- fit$loadings[,1:ncomp,drop=FALSE]
			scores <- fit$scores[,1:ncomp,drop=FALSE]
			sdev <- fit$sdev[1:ncomp]
			list(scores=scores, loadings=loadings, sdev=sdev, # totvar=fit$totvar,
				method=method, ncomp=ncomp, center=fit$center, scale=fit$scale)
		})
		model <- AnnotatedDataFrame(
			data=data.frame(ncomp=sapply(result, function(fit) fit$ncomp)),
			varMetadata=data.frame(
				labelDescription=c(ncomp="Number of Principal Components")))
		featureNames(model) <- .format.data.frame(pData(model))
		names(result) <- .format.data.frame(pData(model))
		.message("PCA: Done.")
		.time.stop()
		new("PCA",
			pixelData=x@pixelData,
			featureData=x@featureData,
			experimentData=x@experimentData,
			protocolData=x@protocolData,
			resultData=result,
			modelData=model)
	})

setMethod("predict", "PCA",
	function(object, newx, ...)
	{
		.time.start()
		.message("PCA: Predicting principal components scores.")
		result <- lapply(object@resultData, function(res) {
			.message("PCA: Predicting scores for ncomp = ", res$ncomp, ".")
			pred <- .PCA.predict(newx, ncomp=res$ncomp, loadings=res$loadings,
				center=res$center, scale=res$scale)
			res[names(pred)] <- pred
			res
		})
		.message("PCA: Done.")
		.time.stop()
		new("PCA",
			pixelData=newx@pixelData,
			featureData=newx@featureData,
			experimentData=newx@experimentData,
			protocolData=newx@protocolData,
			resultData=result,
			modelData=object@modelData)
	})

.PCA.fit <- function(x, ncomp, method, center, scale, iter.max) {
	if ( method == "irlba" ) {
		Xt <- t(as.matrixlike(iData(x), supported=c("matrix", "matter")))
	} else {
		Xt <- t(as.matrixlike(iData(x), supported="matrix"))
	}
	Xt <- scale(Xt, center=center, scale=scale)
	if ( center )
		center <- attr(Xt, "scaled:center")
	if ( scale )
		scale <- attr(Xt, "scaled:scale")
	if ( method == "nipals" ) {
		nipals.out <- nipals.PCA(Xt, ncomp=ncomp, iter.max=iter.max)
		sdev <- nipals.out$sdev
		loadings <- nipals.out$loadings
		scores <- nipals.out$scores
	} else {
		if ( method == "irlba" ) {
			sv <- irlba(Xt, nu=ncomp, nv=ncomp, fastpath=is.matrix(Xt))
		} else if ( method == "svd") {
			sv <- svd(Xt, nu=ncomp, nv=ncomp)
		}
		sdev <- sv$d[1:ncomp] / sqrt(max(1, nrow(Xt) - 1))
		loadings <- sv$v
		scores <- Xt %*% loadings
	}
	# totvar <- sum(apply(Xt, 2, var))
	rownames(loadings) <- featureNames(x)
	colnames(loadings) <- paste("PC", 1:ncomp, sep="")
	rownames(scores) <- pixelNames(x)
	colnames(scores) <- paste("PC", 1:ncomp, sep="")
	list(scores=scores, loadings=loadings, sdev=sdev, # totvar=totvar,
		method=method, center=center, scale=scale)
}

.PCA.predict <- function(x, ncomp, loadings, center, scale) {
	Xt <- t(as.matrixlike(iData(x), supported=c("matrix", "matter")))
	Xt <- scale(Xt, center=center, scale=scale)
	scores <- Xt %*% loadings[,1:ncomp,drop=FALSE]
	sdev <- apply(scores, 2, sd)
	rownames(scores) <- pixelNames(x)
	colnames(scores) <- paste("PC", 1:ncomp, sep="")
	list(scores=scores, loadings=loadings, sdev=sdev)
}

