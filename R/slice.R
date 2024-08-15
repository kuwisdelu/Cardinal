
#### Slice a spectral image  ####
## ------------------------------

slice <- function(x, i = features(x, ...), ...,
	run = NULL, simplify = TRUE, drop = TRUE)
{
	if ( is.null(run) ) {
		coord <- coord(x)
		runs <- run(x)
		xi <- spectra(x)[i,,drop=FALSE]
	} else {
		if ( !is.character(run) && !is.factor(run) )
			run <- runNames(x)[run]
		j <- run(x) %in% run
		coord <- coord(x)[j,,drop=FALSE]
		runs <- droplevels(run(x)[j])
		xi <- spectra(x)[i,j,drop=FALSE]
	}
	ndim <- length(coord)
	FUN <- function(y)
	{
		rs <- lapply(levels(runs),
			function(irun)
			{
				vals <- y[runs %in% irun]
				co <- coord[runs %in% irun,,drop=FALSE]
				if ( ndim == 2L ) {
					to_raster(co$x, co$y, vals)
				} else if ( ndim == 3L ) {
					to_raster3(co$x, co$y, co$z, vals)
				} else {
					.Error("number of coordinates must be 2 or 3")
				}
			})
		names(rs) <- levels(runs)
		if ( simplify )
			rs <- simplify2array(rs)
		rs
	}
	ans <- apply(xi, 1L, FUN, simplify=FALSE)
	if ( is.null(run) ) {
		names(ans) <- featureNames(x)
	} else {
		names(ans) <- featureNames(x)[j]
	}
	if ( simplify ) {
		ans <- simplify2array(ans)
		dnms <- c(coordNames(x), "run", "feature")
		dim(ans) <- setNames(dim(ans), dnms)
		if ( drop )
			ans <- drop(ans)
	}
	ans
}
