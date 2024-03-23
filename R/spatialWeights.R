
#### Find spatial neighbors ####
## -----------------------------

setMethod("spatialWeights", "SpectralImagingExperiment",
	function(x, r = 1,
		neighbors = findNeighbors(x, r=r), matrix = FALSE,
		BPPARAM = getCardinalBPPARAM(), ...)
{
	nb <- neighbors$index
	ds <- colDists(spectra(x), at=nb,
		nchunks=getCardinalNChunks(),
		verbose=getCardinalVerbose(),
		BPPARAM=BPPARAM, ...)
	sds <- lapply(ds, function(d) (max(d) / 2)^2)
	FUN <- function(d, sd) exp(-d^2 / (2 * sd^2))
	wts <- Map(FUN, ds, sds)
	if ( matrix ) {
		sparse_mat(index=nb, data=wts,
			nrow=length(nb), ncol=length(nb), offset=1L)
	} else {
		wts
	}
})

setMethod("spatialWeights", "PositionDataFrame",
	function(x, r = 1,
		neighbors = findNeighbors(x, r=r), matrix = FALSE,
		sd = ((2 * r) + 1) / 4, ...)
{
	spatialWeights(as.matrix(coord(x)), r=r,
		neighbors=neighbors, matrix=matrix, sd=sd, ...)
})

setMethod("spatialWeights", "matrix",
	function(x, coord = x, r = 1,
		neighbors = findNeighbors(coord, r=r), matrix = FALSE,
		sd = ((2 * r) + 1) / 4, ...)
{
	nb <- neighbors$index
	ds <- rowdist_at(x, ix=seq_len(nrow(x)), iy=nb)
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
})
