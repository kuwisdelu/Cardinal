
#### X-Y plotting for MSImagingExperiment ####
## ---------------------------------------------

setMethod("plot",
	signature = c(x = "MSImagingExperiment", y = "formula"),
	function(x, y, ...) {
		plot(x, formula=y, ...)
	})

setMethod("plot",
	signature = c(x = "MSImagingExperiment", y = "missing"),
	function(x, formula,
		pixel = pixels(x, coord=coord),
		pixel.groups,
		coord,
		plusminus,
		...,
		xlab, ylab,
		type = if (centroided(x)) 'h' else 'l')
	{
		if ( !missing(formula) && missing(pixel) && missing(coord) )
			return(callNextMethod(as(x, "SparseImagingExperiment"),
				formula=formula, ..., xlab=xlab, ylab=ylab, type=type))
		if ( missing(formula) && missing(xlab) && missing(ylab) ) {
			xlab <- expression(italic(m/z))
			ylab <- expression(italic(Intensity))
		}
		if ( (!missing(pixel) || !missing(coord)) && missing(pixel.groups) ) {
			if ( missing(coord) )
				coord <- Cardinal::coord(x)[pixel,]
			if ( missing(plusminus) || all(plusminus == 0) ) {
			 	if ( is.null(pixelNames(x)) ) {
			 		pixel.names <- data.frame(run=run(x)[pixel], coord(x)[pixel,])
					pixel.groups <- unname(.format.data.labels(pixel.names))
				} else {
					pixel.groups <- pixelNames(x)[pixel]
				}
			 } else {
			 	coord <- as.data.frame(as.list(coord))
			 	coord.groups <- unname(.format.data.labels(coord,
			 		append=paste0(" \u00b1 ", abs(plusminus))))
			 	run.groups <- .format.data.labels(list(run=runNames(x)))
			 	run.groups <- rep(run.groups, each=length(coord.groups))
			 	pixel.groups <- paste(run.groups, coord.groups, sep=", ")
			 	dxy <- rep_len(abs(plusminus), ncol(coord(x)))
			 	pixel.list <- lapply(runNames(x), function(run) {
			 		lapply(seq_len(nrow(coord)), function(i) {
						p <- mapply(function(pos, coords, dxyi) {
							coords >= pos - dxyi & coords <= pos + dxyi
						}, coord[i,], coord(x), dxy, SIMPLIFY=TRUE)
						p <- which(run == run(x) & apply(as.matrix(p), 1, all))
						if ( length(p) == 0L )
							.warning("no pixels in range; removing ", pixel.groups[i])
						p
					})
		 		})
		 		pixel.list <- unlist(pixel.list, recursive=FALSE)
				pixel.groups <- rep.int(pixel.groups, lengths(pixel.list))
				pixel <- unlist(pixel.list)
			}
		}
		if ( missing(pixel.groups) ) {
			pixel.groups <- NULL
		} else {
			pixel.groups <- .try_eval(substitute(pixel.groups),
				envir=as.env(pixelData(x),
				enclos=environment(formula)))
		}
		callNextMethod(as(x, "SparseImagingExperiment"),
			formula=formula,
			pixel=pixel,
			pixel.groups=pixel.groups,
			...,
			xlab=xlab, ylab=ylab,
			type=type)
	})

