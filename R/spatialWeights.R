
#### Find spatial neighbors ####
## -----------------------------

setMethod("spatialWeights", "ANY",
	function(x, coord = x, r = 1, byrow = TRUE,
		neighbors = findNeighbors(coord, r=r),
		weights = c("gaussian", "adaptive"),
		sd = ((2 * r) + 1) / 4, matrix = FALSE,
		verbose = getCardinalVerbose(), chunkopts = list(),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	wts <- .spatialWeights(coord, byrow=byrow || missing(coord),
		neighbors=neighbors, weights="gaussian", sd=sd,
		verbose=verbose, chunkopts=chunkopts,
		BPPARAM=BPPARAM)
	if ( match.arg(weights) == "adaptive" )
	{
		awts <- .spatialWeights(x, byrow=byrow,
			neighbors=neighbors, weights="adaptive", sd=sd,
			verbose=verbose, chunkopts=chunkopts,
			BPPARAM=BPPARAM)
		wts <- Map("*", wts, awts)
	}
	if ( matrix ) {
		sparse_mat(index=neighbors, data=wts,
			nrow=length(neighbors), ncol=length(neighbors),
			offset=1L)
	} else {
		wts
	}
})

setMethod("spatialWeights", "SpectralImagingExperiment",
	function(x, r = 1,
		neighbors = findNeighbors(x, r=r),
		weights = c("gaussian", "adaptive"), ...)
{
	spatialWeights(spectra(x), coord=coord(x), r=r, byrow=FALSE,
		neighbors=neighbors, weights=weights, ...)
})

setMethod("spatialWeights", "PositionDataFrame",
	function(x, r = 1,
		neighbors = findNeighbors(x, r=r),
		weights = c("gaussian", "adaptive"), ...)
{
	spatialWeights(dropkeys(x), coord=coord(x), r=r, byrow=TRUE,
		neighbors=neighbors, weights=weights, ...)
})

.spatialWeights <- function(x,
	neighbors, weights, sd, byrow,
	verbose, chunkopts, BPPARAM)
{
	if ( byrow ) {
		if ( is.matrix(x) || is.data.frame(x) ) {
			ds <- rowdist_at(x, ix=seq_len(nrow(x)), iy=neighbors)
		} else {
			ds <- rowDists(x, at=neighbors,
				verbose=verbose, chunkopts=chunkopts,
				BPPARAM=BPPARAM)
		}
	} else {
		if ( is.matrix(x) || is.data.frame(x) ) {
			ds <- coldist_at(x, ix=seq_len(ncol(x)), iy=neighbors)
		} else {
			ds <- colDists(x, at=neighbors,
				verbose=verbose, chunkopts=chunkopts,
				BPPARAM=BPPARAM)
		}
	}
	if ( weights == "adaptive" ) {
		sds <- vapply(ds, function(d) (max(d) / 2)^2, numeric(1L))
	} else {
		sds <- rep_len(sd, nrow(x))
	}
	FUN <- function(d, sd) exp(-d^2 / (2 * sd^2))
	Map(FUN, ds, sds)
}

