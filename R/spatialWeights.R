
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
	.spatialWeights(x, neighbors=neighbors,
		weights=weights, sd=sd, byrow=byrow, matrix=matrix,
		verbose=verbose, chunkopts=chunkopts,
		BPPARAM=BPPARAM)
})

setMethod("spatialWeights", "SpectralImagingExperiment",
	function(x, r = 1,
		neighbors = findNeighbors(x, r=r), ...)
{
	spatialWeights(spectra(x), neighbors=neighbors,
		weights="adaptive", byrow=FALSE, ...)
})

setMethod("spatialWeights", "PositionDataFrame",
	function(x, r = 1,
		neighbors = findNeighbors(x, r=r),
		sd = ((2 * r) + 1) / 4, ...)
{
	spatialWeights(as.matrix(coord(x)), neighbors=neighbors,
		weights="gaussian", sd=sd, ...)
})

.spatialWeights <- function(x,
	neighbors, weights, sd, byrow, matrix,
	verbose, chunkopts, BPPARAM)
{
	weights <- match.arg(weights, c("gaussian", "adaptive"))
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
	wts <- Map(FUN, ds, sds)
	if ( matrix ) {
		sparse_mat(index=neighbors, data=wts,
			nrow=length(neighbors), ncol=length(neighbors),
			offset=1L)
	} else {
		wts
	}
}

