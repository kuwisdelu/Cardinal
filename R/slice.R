
#### Slice a spectral image  ####
## ------------------------------

slice <- function(x, i = features(x, ...), ...,
	simplify = TRUE, drop = TRUE)
{
	coord <- coord(x)
	ndim <- length(coord)
	FUN <- function(y)
	{
		rs <- lapply(levels(run(x)),
			function(irun)
			{
				vals <- y[run(x) %in% irun]
				coord <- coord(x)[run(x) %in% irun,,drop=FALSE]
				if ( ndim == 2L ) {
					to_raster(coord$x, coord$y, vals)
				} else if ( ndim == 3L ) {
					to_raster3(coord$x, coord$y, coord$z, vals)
				} else {
					stop("number of coordinates must be 2 or 3")
				}
			})
		names(rs) <- levels(run(x))
		if ( simplify )
			rs <- simplify2array(rs)
		rs
	}
	xi <- spectra(x)[i,,drop=FALSE]
	ans <- apply(xi, 1L, FUN, simplify=FALSE)
	if ( simplify ) {
		ans <- simplify2array(ans)
		if ( is.array(ans) ) {
			ndim <- length(dim(ans))
			perm <- c(ndim, seq_len(ndim - 1L))
			ans <- aperm(ans, perm=perm)
		}
		if ( drop )
			ans <- drop(ans)
	}
	ans
}
