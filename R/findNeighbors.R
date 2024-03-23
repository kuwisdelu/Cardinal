
#### Find spatial neighbors ####
## -----------------------------

setMethod("findNeighbors", "SpectralImagingData",
	function(x, r = 1, groups = run(x),
		metric = "maximum", p = 2, matrix = FALSE, ...)
{
	.findNeighbors(coord(x), r=r, groups=groups,
		metric=metric, p=p, matrix=matrix)
})

setMethod("findNeighbors", "PositionDataFrame",
	function(x, r = 1, groups = run(x),
		metric = "maximum", p = 2, matrix = FALSE, ...)
{
	.findNeighbors(coord(x), r=r, groups=groups,
		metric=metric, p=p, matrix=matrix)
})

setMethod("findNeighbors", "ANY",
	function(x, r = 1, groups = NULL,
		metric = "maximum", p = 2, matrix = FALSE, ...)
{
	.findNeighbors(x, r=r, groups=groups,
		metric=metric, p=p, matrix=matrix)
})

.findNeighbors <- function(x, r, groups, metric, p, matrix)
{
	x <- as.matrix(x)
	if ( is.null(groups) ) {
		groups <- rep.int(1L, nrow(x))
	} else {
		groups <- rep_len(groups, nrow(x))
	}
	nb <- kdsearch(x, x, tol=r)
	ds <- rowdist_at(x, ix=seq_len(nrow(x)), iy=nb, metric=metric, p=p)
	for ( i in seq_len(nrow(x)) )
	{
		ok <- ds[[i]] <= r & groups[nb[[i]]] %in% groups[i]
		nb[[i]] <- nb[[i]][ok]
		ds[[i]] <- ds[[i]][ok]
	}
	if ( matrix ) {
		ones <- lapply(nb, function(i) rep_len(1L, length(i)))
		sparse_mat(index=nb, data=ones,
			nrow=length(nb), ncol=length(nb), offset=1L)
	} else {
		nb
	}
}
