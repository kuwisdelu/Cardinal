
## Slice an imaging dataset (as a data cube)

setMethod("slice", "SparseImagingExperiment", function(x, ..., drop = TRUE)
	{
		obj <- x
		i <- features(obj, ...)
		values <- iData(obj)[i,,drop=FALSE]
		x <- coord(obj)[,1]
		y <- coord(obj)[,2]
		if ( length(coord(obj)) > 2L ) {
			dnm <- list(NULL, NULL, NULL)
			nms <- coordnames(obj)[1:3]
			names(dnm) <- nms
			if ( gridded(pData(obj)) ) {
				dim <- dims(pData(obj))[1:3]
				res <- resolution(pData(obj))[1:3]
			} else {
				dim <- NULL
				res <- NULL
			}
			z <- coord(obj)[,3]
		} else {
			dnm <- list(NULL, NULL, runNames(obj))
			nms <- c(coordnames(obj)[c(1,2)], "run")
			names(dnm) <- nms
			if ( gridded(pData(obj)) ) {
				dim <- c(dims(pData(obj))[c(1,2)], nlevels(run(obj)))
				res <- c(resolution(pData(obj))[c(1,2)], 1)
			} else {
				dim <- NULL
				res <- NULL
			}
			z <- as.integer(run(obj))
		}
		dnm <- c(dnm, list(feature=NULL))
		a <- lapply(1:nrow(values), function(i2) {
			vals <- as.numeric(values[i2,])
			projectToRaster3d(x, y, z, vals, dim=dim, res=res)
		})
		a <- simplify2array(a)
		names(dim(a)) <- names(dnm)
		dimnames(a) <- dnm
		if ( drop )
			a <- drop(a)
		a
	})

