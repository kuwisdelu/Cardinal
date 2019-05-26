
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
		pixel = pixels(x, coord=coord, run=run),
		pixel.groups,
		coord,
		run,
		plusminus,
		...,
		xlab, ylab,
		type = if ( is_centroided ) 'h' else 'l')
	{
		is_centroided <- isTRUE(centroided(x))
		if ( !missing(formula) && missing(pixel) && missing(coord) && missing(run) )
			return(callNextMethod(as(x, "SparseImagingExperiment"),
				formula=formula, ..., xlab=xlab, ylab=ylab, type=type))
		if ( missing(xlab) && missing(ylab) ) {
			if ( missing(formula) || is.null(.parseFormula2(formula)$lhs) ) {
				xlab <- expression(italic(m/z))
				ylab <- expression(italic(Intensity))
			}
		}
		if ( (!missing(pixel) || !missing(coord)) && missing(pixel.groups) ) {
			if ( missing(coord) )
				coord <- Cardinal::coord(x)[pixel,]
			if ( missing(plusminus) || all(plusminus == 0) ) {
			 	if ( is.null(pixelNames(x)) ) {
			 		pixel.names <- data.frame(run=Cardinal::run(x)[pixel], coord(x)[pixel,])
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
						p <- which(run == Cardinal::run(x) & apply(as.matrix(p), 1, all))
						if ( length(p) == 0L )
							.warning("no pixels in range; removing ", pixel.groups[i])
						p
					})
		 		})
		 		pixel.list <- unlist(pixel.list, recursive=FALSE)
				pixel.groups <- rep.int(pixel.groups, lengths(pixel.list))
				if ( !missing(run) ) {
					pixel <- intersect(unlist(pixel), pixels(x, run=run))
				} else {
					pixel <- unlist(pixel.list)
				}
			}
		} else if ( !missing(run) && missing(pixel.groups) ) {
			pixel.groups <- Cardinal::run(x)
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

