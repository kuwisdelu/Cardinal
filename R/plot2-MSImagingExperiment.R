
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
		type = if (centroided(x)) 'h' else 'l')
	{
		if ( !missing(formula) && missing(pixel) && missing(coord) )
			return(callNextMethod(x, formula=formula, ..., type=type))
		if ( (!missing(coord) || length(pixel) == 1L) && missing(pixel.groups) ) {
			if ( missing(coord) )
				coord <- Cardinal::coord(x)[pixel,]
			if ( missing(plusminus) || all(plusminus == 0) ) {
			 	if ( is.null(pixelNames(x)) ) {
					pixel.groups <- unname(.format.data.labels(coord(x)[pixel,]))
				} else {
					pixel.groups <- pixelNames(x)[pixel]
				}
			 } else {
			 	coord <- as.data.frame(coord)
			 	pixel.groups <- unname(.format.data.labels(coord,
			 		append=paste0(" \u00b1 ", abs(plusminus))))
			 	dxy <- rep_len(abs(plusminus), ncol(coord(x)))
				pixel.list <- lapply(seq_len(nrow(coord)), function(i) {
					p <- mapply(function(xyi, coordi, dxyi) {
						coordi >= xyi - dxyi & coordi <= xyi + dxyi
					}, coord[i,], coord(x), dxy, SIMPLIFY=TRUE)
					p <- which(apply(as.matrix(p), 1, all))
					if ( length(p) == 0L )
						.warning("no pixels in range; removing ", pixel.groups[i])
					p
				})
				pixel.groups <- rep.int(pixel.groups, lengths(pixel.list))
				pixel <- unlist(pixel.list)
			}
		}
		if ( missing(pixel.groups) || !missing(formula) )
			pixel.groups <- NULL
		callNextMethod(x,
			formula=formula,
			pixel=pixel,
			pixel.groups=pixel.groups,
			...,
			type=type)
	})

