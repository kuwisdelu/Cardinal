
#### Find spatial neighbors ####
## -----------------------------

setMethod("findNeighbors", "SpectralImagingData",
	function(x, r = 1, groups = run(x), metric = "maximum", p = 2,
		offsets = TRUE, matrix = FALSE, ...)
{
	.findNeighbors(coord(x), r=r, groups=groups, metric=metric, p=p,
		offsets=offsets, matrix=matrix)
})

setMethod("findNeighbors", "PositionDataFrame",
	function(x, r = 1, groups = run(x), metric = "maximum", p = 2,
		offsets = TRUE, matrix = FALSE, ...)
{
	.findNeighbors(coord(x), r=r, groups=groups, metric=metric, p=p,
		offsets=offsets, matrix=matrix)
})

setMethod("findNeighbors", "ANY",
	function(x, r = 1, groups = NULL, metric = "maximum", p = 2,
		offsets = TRUE, matrix = FALSE, ...)
{
	.findNeighbors(x, r=r, groups=groups, metric=metric, p=p,
		offsets=offsets, matrix=matrix)
})

.findNeighbors <- function(x, r, groups, metric, p, offsets, matrix)
{
	x <- as.matrix(x)
	if ( is.null(groups) ) {
		groups <- rep.int(1L, nrow(x))
	} else {
		groups <- rep_len(groups, nrow(x))
	}
	nb <- kdsearch(x, x, tol=r)
	ds <- rowdist_at(x, ix=seq_len(nrow(x)), iy=nb, metric=metric, p=p)
	dx <- vector("list", nrow(x))
	for ( i in seq_len(nrow(x)) )
	{
		ok <- ds[[i]] <= r & groups[nb[[i]]] %in% groups[i]
		nb[[i]] <- nb[[i]][ok]
		ds[[i]] <- ds[[i]][ok]
		if ( offsets ) {
			ii <- rep.int(i, length(nb[[i]]))
			dx[[i]] <- x[nb[[i]],,drop=FALSE] - x[ii,,drop=TRUE]
		}
	}
	if ( matrix ) {
		ones <- lapply(nb, function(i) rep_len(1L, length(i)))
		sparse_mat(index=nb, data=ones,
			nrow=length(nb), ncol=length(nb), offset=1L)
	} else {
		if ( offsets ) {
			DataFrame(index=I(nb), dist=I(ds), offset=I(dx))
		} else {
			DataFrame(index=I(nb), dist=I(ds))
		}
	}
}
