
#### Find spatial neighbors ####
## -----------------------------

setMethod("spatialWeights", "SpectralImagingExperiment",
	function(x, r = 1,
		neighbors = findNeighbors(x, r=r),
		sd = NULL, matrix = FALSE,
		BPPARAM = getCardinalBPPARAM(), ...)
{
	.spatialWeights(spectra(x), neighbors=neighbors,
		transpose=TRUE, sd=sd, matrix=matrix, BPPARAM=BPPARAM)
})

setMethod("spatialWeights", "PositionDataFrame",
	function(x, r = 1,
		neighbors = findNeighbors(x, r=r),
		sd = ((2 * r) + 1) / 4, matrix = FALSE,
		BPPARAM = getCardinalBPPARAM(), ...)
{
	.spatialWeights(as.matrix(coord(x)), neighbors=neighbors,
		transpose=FALSE, sd=sd, matrix=matrix, BPPARAM=BPPARAM)
})

setMethod("spatialWeights", "ANY",
	function(x, coord = x, r = 1,
		neighbors = findNeighbors(coord, r=r),
		sd = ((2 * r) + 1) / 4,
		transpose = FALSE, matrix = FALSE,
		BPPARAM = getCardinalBPPARAM(), ...)
{
	.spatialWeights(x, neighbors=neighbors,
		transpose=transpose, sd=sd, matrix=matrix, BPPARAM=BPPARAM)
})

.spatialWeights <- function(x, neighbors,
	transpose, sd, matrix, BPPARAM)
{
	nb <- neighbors$index
	if ( transpose ) {
		if ( is.matrix(x) || is.data.frame(x) ) {
			ds <- coldist_at(x, ix=seq_len(ncol(x)), iy=nb)
		} else {
			ds <- colDists(x, at=nb,
				nchunks=getCardinalNChunks(),
				verbose=getCardinalVerbose(),
				BPPARAM=BPPARAM)
		}
	} else {
		if ( is.matrix(x) || is.data.frame(x) ) {
			ds <- rowdist_at(x, ix=seq_len(nrow(x)), iy=nb)
		} else {
			ds <- rowDists(x, at=nb,
				nchunks=getCardinalNChunks(),
				verbose=getCardinalVerbose(),
				BPPARAM=BPPARAM)
		}
	}
	if ( is.null(sd) ) {
		sds <- vapply(ds, function(d) (max(d) / 2)^2, numeric(1L))
	} else {
		sds <- rep_len(sd, nrow(x))
	}
	FUN <- function(d, sd) exp(-d^2 / (2 * sd^2))
	wts <- Map(FUN, ds, sds)
	if ( matrix ) {
		sparse_mat(index=nb, data=wts,
			nrow=length(nb), ncol=length(nb), offset=1L)
	} else {
		wts
	}
}

