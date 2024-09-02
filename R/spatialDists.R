
#### Find spatial neighbors ####
## -----------------------------

setMethod("spatialDists", "ANY",
	function(x, y, coord, r = 1, byrow = TRUE,
		metric = "euclidean", p = 2, weights = NULL,
		neighbors = findNeighbors(coord, r=r),
		neighbors.weights = spatialWeights(coord, r=r),
		verbose = getCardinalVerbose(), chunkopts = list(),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	.spatialDists(x, y, byrow=byrow,
		metric=metric, p=p, weights=weights,
		neighbors=neighbors, neighbors.weights=neighbors.weights,
		verbose=verbose, chunkopts=chunkopts,
		BPPARAM=BPPARAM)
})

setMethod("spatialDists", "SpectralImagingExperiment",
	function(x, y, r = 1,
		neighbors = findNeighbors(x, r=r),
		neighbors.weights = spatialWeights(x, r=r), ...)
{
	spatialDists(spectra(x), y, byrow=FALSE,
		neighbors=neighbors,
		neighbors.weights=neighbors.weights, ...)
})

setMethod("spatialDists", "PositionDataFrame",
	function(x, y, r = 1,
		neighbors = findNeighbors(x, r=r),
		neighbors.weights = spatialWeights(x, r=r), ...)
{
	spatialDists(as.matrix(dropkeys(x)), y, byrow=TRUE,
		neighbors=neighbors,
		neighbors.weights=neighbors.weights, ...)
})

.spatialRowDists <- function(x, y, ...) {
	spatialDists(x, y, byrow=TRUE, ...)
}

.spatialColDists <- function(x, y, ...) {
	spatialDists(x, y, byrow=FALSE, ...)
}

.spatialDists <- function(x, y, metric, p, weights,
	neighbors, neighbors.weights, byrow,
	verbose, chunkopts, BPPARAM)
{
	if ( byrow ) {
		ds <- rowDists(x, y,
			metric=metric, p=p, weights=weights,
			verbose=verbose, chunkopts=chunkopts,
			BPPARAM=BPPARAM)
	} else {
		ds <- colDists(x, y,
			metric=metric, p=p, weights=weights,
			verbose=verbose, chunkopts=chunkopts,
			BPPARAM=BPPARAM)
	}
	FUN <- function(nb, w) colSums(w * ds[nb,,drop=FALSE]) / sum(w)
	ans <- mapply(FUN, neighbors, neighbors.weights)
	if ( is.matrix(ans) ) {
		ans <- t(ans)
	} else {
		ans <- as.matrix(ans)
	}
	dimnames(ans) <- dimnames(y)
	ans
}

