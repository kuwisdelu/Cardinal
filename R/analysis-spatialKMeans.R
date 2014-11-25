
setMethod("spatialKMeans",
	signature = c(x = "SImageSet"),
	function(x, r = 1, k = 2,
		method = c("gaussian", "adaptive"),
		weights = 1, iter.max = 100, nstart = 100,
		algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
			"MacQueen"),
		ncomp = 20, ...)
	{
		method <- match.arg(method)
		save.seed()
		rs <- sort(r)
		ks <- sort(k)
		w <- rep(weights, length.out=nrow(iData(x)))
		.time.start()
		result <- unlist(lapply(rs, function(r){
			spatial <- spatial.info(x, r=r, method=method)
			.message("spatialKMeans: Calculating spatial information for r = ", r, ".")
			fastmap <- fastmap.spatial(iData(x), r=r, spatial=spatial, w=w, ncomp=ncomp)
			lapply(ks, function(k) {
				.message("spatialKMeans: Fitting r = ", r, ", k = ", k, ".")
				append(.spatialKMeans(x, fastmap=fastmap, k=k,
					iter.max=iter.max, nstart=nstart, algorithm=algorithm),
					list(r=r, k=k, method=method, weights=weights,
						fastmap=fastmap))
			})
		}), recursive=FALSE)
		.message("spatialKMeans: Preparing results.")
		model <- AnnotatedDataFrame(data=data.frame(
				r=sapply(result, function(fit) fit$r),
				k=sapply(result, function(fit) fit$k)),
			varMetadata=data.frame(
				labelDescription=c(
					k="Number of clusters",
					r="Neighborhood radius")))
		featureNames(model) <- .format.data.frame(pData(model))
		names(result) <- .format.data.frame(pData(model))
		object <- new("SpatialKMeans",
			pixelData=x@pixelData,
			featureData=x@featureData,
			experimentData=x@experimentData,
			protocolData=x@protocolData,
			resultData=result,
			modelData=model)
		object <- coregister(object)
		.message("spatialKMeans: Done.")
		.time.stop()
		object
	})

.spatialKMeans <- function(x, fastmap, k, iter.max, nstart, algorithm) {
	restore.seed()
	start.time <- proc.time()
	cluster <- kmeans(fastmap$scores, centers=k,
		iter.max=iter.max, nstart=nstart,
		algorithm=algorithm)$cluster
	centers <- sapply(sort(unique(cluster)), function(Ck) {
		rowMeans(iData(x)[,cluster==Ck,drop=FALSE])
	})
	cluster <- factor(cluster)
	withinss <- sapply(levels(cluster), function(Ck) {
		apply(scale(t(iData(x)[,cluster==Ck]), scale=FALSE)^2, 2, sum)
	})
	totss <- apply(scale(t(iData(x)), scale=FALSE)^2, 2, sum)
	betweenss <- totss - rowSums(withinss)
	names(cluster) <- pixelNames(x)
	rownames(centers) <- featureNames(x)
	colnames(centers) <- levels(cluster)
	rownames(withinss) <- featureNames(x)
	colnames(withinss) <- levels(cluster)
	names(betweenss) <- featureNames(x)
	names(totss) <- featureNames(x)
	list(cluster=cluster, centers=centers, totss=totss,
		withinss=withinss, betweenss=betweenss,
		time=proc.time() - start.time)
}

.spatialKMeans.reclass <- function(x, ref) {
	relevel <- x$cluster
	levels(relevel) <- levels(x$cluster)[ref]
	x$cluster <- factor(relevel,
		levels=levels(x$cluster),
		labels=levels(x$cluster))
	x$centers <- x$centers[,order(ref)]
	colnames(x$centers) <- levels(x$cluster)
	x
}
