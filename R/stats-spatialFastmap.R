

### implement methods for FastMap ####

setMethod("spatialFastmap", signature = c(x = "SImageSet"),
	function(x, r = 1, ncomp = 2,
		method = c("gaussian", "adaptive"),
		iter.max = 1, ...)
	{
		method <- match.arg(method)
		rs <- sort(r)
		ncomps <- sort(ncomp)
		if ( max(ncomps) > nrow(x) )
			.stop("spatialFastmap: Can't fit more components than extent of dataset")
		.time.start()
		.message("spatialFastmap: Fitting FastMap components.")
		if ( max(ncomps) > nrow(x) )
			.stop("spatialFastmap: Can't fit more components than extent of dataset")
		result <- unlist(lapply(rs, function(r) {
			.message("spatialFastmap: Fitting r = ", r)
			fit <- .spatialFastmap(x, r=r, ncomp=max(ncomps),
				method=method, iter.max=iter.max)
			lapply(ncomps, function(ncomp) {
				scores <- fit$scores[,1:ncomp,drop=FALSE]
				cor <- fit$cor[,1:ncomp,drop=FALSE]
				pivot.array <- fit$pivot.array[1:ncomp,,drop=FALSE]
				sdev <- fit$sdev[1:ncomp]
				list(scores=scores, cor=cor,
					pivot.array=pivot.array,
					sdev=sdev, r=r, ncomp=ncomp)
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

.spatialFastmap <- function(x, r, ncomp, method, ...) {
	distfun <- .spatialDistanceFun(x, r, method)
	fmap <- fastmap(x, distfun=distfun, ncomp=ncomp, ...)
	fmap$cor <- t(apply(iData(x), 1, function(xi) {
		vapply(1:ncomp, function(nc) {
			si <- fmap$scores[,nc]
			if ( all(si == 0) ) {
				0
			} else {
				cor(xi, si)
			}
		}, numeric(1))
	}))
	fmap <- fmap[c("scores", "cor", "pivot.array")]
	colnames(fmap$scores) <- paste("FC", 1:ncomp, sep="")
	colnames(fmap$cor) <- paste("FC", 1:ncomp, sep="")
	fmap$pivot.array <- as.data.frame(fmap$pivot.array)
	names(fmap$pivot.array) <- c("Oa", "Ob")
	fmap$sdev <- apply(fmap$scores, 2, sd)
	fmap
}

.spatialDistanceFun <- function(x, r, method) {
	bilateral <- switch(method,
		gaussian=FALSE, adaptive=TRUE)
	sigma <- ((2 * mean(r)) + 1) / 4
	neighbors <- findNeighbors(x, r=r)
	coord <- as.matrix(coord(x)[,coordLabels(x),drop=FALSE])
	offsets <- lapply(1:ncol(x), function(i) {
		.findSpatialOffsets(coord, neighbors, i)
	})
	weights <- mapply(function(ii, pos) {
		xi <- iData(x)[,ii]
		.findSpatialWeights(xi, pos, sigma, bilateral=bilateral)
	}, neighbors, offsets, SIMPLIFY=FALSE)
	X <- iData(x)
	function(x, i, j) {
		xi <- X[,neighbors[[i]],drop=FALSE]
		xj <- X[,neighbors[[j]],drop=FALSE]
		.findSpatialDistance(xi, xj, offsets[[i]], offsets[[j]],
			weights[[i]], weights[[j]], sigma)
	}
}
