
setMethod("spatialFastmap", signature = c(x = "SImageSet"),
	function(x, r = 1, ncomp = 3,
		method = c("gaussian", "adaptive"),
		metric = c("average", "correlation", "neighborhood"),
		iter.max = 1, ...)
	{
		.Deprecated_Cardinal1()
		method <- match.arg(method)
		metric <- match.arg(metric)
		iData(x) <- as.matrix(iData(x))
		rs <- sort(r)
		ncomps <- sort(ncomp)
		if ( max(ncomps) > nrow(x) )
			.stop("spatialFastmap: Can't fit more components than extent of dataset")
		.time.start()
		.message("spatialFastmap: Fitting FastMap components.")
		result <- unlist(lapply(rs, function(r) {
			.message("spatialFastmap: Fitting r = ", r)
			fit <- .spatialFastmap(x, r=r, ncomp=max(ncomps),
				method=method, metric=metric, iter.max=iter.max)
			lapply(ncomps, function(ncomp) {
				scores <- fit$scores[,1:ncomp,drop=FALSE]
				correlation <- fit$correlation[,1:ncomp,drop=FALSE]
				pivot.array <- fit$pivot.array[1:ncomp,,drop=FALSE]
				sdev <- fit$sdev[1:ncomp]
				list(scores=scores, correlation=correlation,
					pivots=pivot.array, sdev=sdev,
					r=r, ncomp=ncomp)
			})
		}), recursive=FALSE)
		model <- AnnotatedDataFrame(data=data.frame(
				r=sapply(result, function(fit) fit$r),
				ncomp=sapply(result, function(fit) fit$ncomp)),
			varMetadata=data.frame(
				labelDescription=c(
					r="Neighborhood radius",
					ncomp="Number of FastMap Components")))
		featureNames(model) <- .format.data.labels(pData(model))
		names(result) <- .format.data.labels(pData(model))
		.message("spatialFastmap: Done.")
		.time.stop()
		new("SpatialFastmap",
			pixelData=x@pixelData,
			featureData=x@featureData,
			experimentData=x@experimentData,
			protocolData=x@protocolData,
			resultData=result,
			modelData=model)
	})

.spatialFastmap <- function(x, r, ncomp, method, metric, ...) {
	distfun <- .spatialDistanceFun(x, r, method, metric)
	fmap <- fastmap(x, distfun=distfun, ncomp=ncomp, ...)
	fmap$correlation <- t(apply(iData(x), 1, function(xi) {
		vapply(1:ncomp, function(nc) {
			si <- fmap$scores[,nc]
			if ( all(si == 0) ) {
				0
			} else {
				cor(xi, si)
			}
		}, numeric(1))
	}))
	fmap <- fmap[c("scores", "correlation", "pivot.array")]
	colnames(fmap$scores) <- paste("FC", 1:ncomp, sep="")
	colnames(fmap$correlation) <- paste("FC", 1:ncomp, sep="")
	fmap$pivot.array <- as.data.frame(fmap$pivot.array)
	names(fmap$pivot.array) <- c("Oa", "Ob")
	fmap$sdev <- apply(fmap$scores, 2, sd)
	fmap
}

.spatialDistanceFun <- function(x, r, method, metric) {
	spatial <- .spatialInfo(x, r=r, method=method)
	X <- iData(x)
	function(x, i, j) {
		xi <- X[,spatial$neighbors[[i]],drop=FALSE]
		xj <- X[,spatial$neighbors[[j]],drop=FALSE]
		.spatialDistance(xi, xj,
			offsets=list(spatial$offsets[[i]]),
			weights=list(spatial$weights[[i]]),
			ref.offsets=spatial$offsets[[j]],
			ref.weights=spatial$weights[[j]],
			neighbors=list(seq_len(ncol(xi))),
			metric=metric)
	}
}

.spatialInfo <- function(x, r, dist = "chebyshev",
	weights = TRUE, method = c("gaussian", "adaptive"), ...)
{
	if ( length(r) > 1L ) {
		.warning("r has length > 1; using r = ", r[1])
		r <- r[1]
	}
	neighbors <- findNeighbors(x, r=r, offsets=TRUE, dist=dist)
	offsets <- attr(neighbors, "offsets")
	attr(neighbors, "offsets") <- NULL
	if ( weights ) {
		progress <- getOption("Cardinal.progress")
		options(Cardinal.progress=FALSE)
		weights <- spatialWeights(x, r=r, method=method, ...)
		options(Cardinal.progress=progress)
		list(neighbors=neighbors, offsets=offsets, weights=weights, r=r)
	} else {
		list(neighbors=neighbors, offsets=offsets, r=r)
	}
}
