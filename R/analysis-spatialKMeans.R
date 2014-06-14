
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
	out <- unlist(lapply(rs, function(r){
		spatial <- spatial.info(x, r=r, method=method)
		.message("spatialKMeans: Calculating spatial information for r = ", r, ".")
		fastmap <- fastmap.spatial(iData(x), r=r, spatial=spatial, w=w, ncomp=ncomp)
		lapply(ks, function(k) {
			.message("spatialKMeans: Fitting r = ", r, ", k = ", k, ".")
			res <- append(.spatialKMeans(x, fastmap=fastmap, k=k,
				iter.max=iter.max, nstart=nstart, algorithm=algorithm, ...),
				list(r=r, k=k, method=method, weights=weights,
					fastmap=fastmap))
			class(res) <- "ResultData"
			res
		})
	}), recursive=FALSE)
	.message("spatialKMeans: Preparing results.")
	par <- AnnotatedDataFrame(data=data.frame(
			r=sapply(out, function(fit) fit$r),
			k=sapply(out, function(fit) fit$k)),
		varMetadata=data.frame(
			labelDescription=c(
				k="Number of clusters",
				r="Neighborhood radius")))
	featureNames(par) <- formatParam(pData(par))
	names(out) <- formatParam(pData(par))
	.message("spatialKMeans: Done.")
	.time.stop()
	new("SpatialKMeans",
		pixelData=x@pixelData,
		featureData=x@featureData,
		experimentData=x@experimentData,
		protocolData=x@protocolData,
		resultData=out,
		modelData=par)
})

.spatialKMeans <- function(x, fastmap, k, iter.max, nstart, algorithm, ...) {
	restore.seed()
	start.time <- proc.time()
	cluster <- kmeans(fastmap$scores, centers=k,
		iter.max=iter.max, nstart=nstart,
		algorithm=algorithm)$cluster
	centers <- sapply(sort(unique(cluster)), function(Ck) {
		rowMeans(iData(x)[,cluster==Ck,drop=FALSE])
	})
	cluster <- factor(cluster)
	names(cluster) <- pixelNames(x)
	rownames(centers) <- featureNames(x)
	colnames(centers) <- levels(x)
	list(cluster=cluster, centers=centers, time=proc.time() - start.time)
}
