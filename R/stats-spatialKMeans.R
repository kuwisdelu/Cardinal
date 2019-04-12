
setMethod("spatialKMeans",
	signature = c(x = "SImageSet"),
	function(x, r = 1, k = 3,
		method = c("gaussian", "adaptive"),
		iter.max = 10, nstart = 1,
		algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
			"MacQueen"),
		ncomp = 10, ...)
	{
		method <- match.arg(method)
		iData(x) <- as.matrix(iData(x))
		oseed <- getRNGStream()
		rs <- sort(r)
		ks <- sort(k)
		ncomp <- min(ncomp, nrow(x))
		if ( "weights" %in% names(list(...)) )
			.warning("'weights' is deprecated.\n")
		.time.start()
		result <- unlist(lapply(rs, function(r){
			.message("spatialKMeans: Initializing clusters for r = ", r, ".")
			fastmap <- spatialFastmap(x, r=r, ncomp=ncomp, method=method, iter.max=1)
			fastmap <- resultData(fastmap)[[1]][c("scores", "pivot.array")]
			lapply(ks, function(k) {
				.message("spatialKMeans: Fitting r = ", r, ", k = ", k, ".")
				append(.spatialKMeans(x, k=k, fastmap=fastmap, seed=oseed,
					iter.max=iter.max, nstart=nstart, algorithm=algorithm),
					list(r=r, k=k, method=method, fastmap=fastmap))
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
		featureNames(model) <- .format.data.labels(pData(model))
		names(result) <- .format.data.labels(pData(model))
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

.spatialKMeans <- function(x, k, fastmap, seed, iter.max, nstart, algorithm) {
	start.time <- proc.time()
	setRNGStream(seed)
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
