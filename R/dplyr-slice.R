
## Slice an imaging dataset (as a data cube)

slice.SparseImagingExperiment <- function(.data, ..., .preserve = FALSE)
	{
		i <- features(.data, ...)
		values <- iData(.data)[i,,drop=FALSE]
		x <- coord(.data)[,1]
		y <- coord(.data)[,2]
		if ( length(coord(.data)) > 2L ) {
			dnm <- list(NULL, NULL, NULL)
			nms <- coordnames(.data)[1:3]
			names(dnm) <- nms
			if ( gridded(pData(.data)) ) {
				dim <- dims(pData(.data))[1:3]
				res <- resolution(pData(.data))[1:3]
			} else {
				dim <- NULL
				res <- NULL
			}
			z <- coord(.data)[,3]
		} else {
			dnm <- list(NULL, NULL, runNames(.data))
			nms <- c(coordnames(.data)[c(1,2)], "run")
			names(dnm) <- nms
			if ( gridded(pData(.data)) ) {
				dim <- c(dims(pData(.data))[c(1,2)], nlevels(run(.data)))
				res <- c(resolution(pData(.data))[c(1,2)], 1)
			} else {
				dim <- NULL
				res <- NULL
			}
			z <- as.integer(run(.data))
		}
		dnm <- c(dnm, list(feature=NULL))
		a <- lapply(1:nrow(values), function(i2) {
			vals <- as.numeric(values[i2,])
			projectToRaster3d(x, y, z, vals, dim=dim, res=res)
		})
		a <- simplify2array(a)
		names(dim(a)) <- names(dnm)
		dimnames(a) <- dnm
		if ( !.preserve )
			a <- drop(a)
		a
	}

