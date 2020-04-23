
#### Plotting for MSImageSet ####

setMethod("plot",
	signature = c(x = "MSImageSet", y = "missing"),
	function(x, formula = ~ mz,
		pixel = pixels(x, coord=coord),
		pixel.groups,
		coord,
		plusminus,
		...,
		type = if (centroided(x)) 'h' else 'l')
	{
		.Deprecated_Cardinal1()
		if ( !missing(coord) && missing(pixel.groups) ) {
			 pixel.groups <- pixelNames(x)[pixel]
			 if ( !missing(plusminus) && plusminus != 0 ) {
			 	newpixels <- findNeighbors(x, r=plusminus)[pixel]
				pixel.groups <- rep.int(pixel.groups, lengths(newpixels))
				pixel <- unlist(newpixels)
			}
		} else if ( missing(pixel.groups) ) {
			pixel.groups <- NULL
		}
		if ( missing(pixel) && missing(coord) && missing(formula) )
			.stop("plot: 'pixel' or 'coord' must be specified")
		callNextMethod(x,
			formula=formula,
			pixel=pixel,
			pixel.groups=pixel.groups,
			...,
			type=type)
	})

setMethod("plot",
	signature = c(x = "MSImageSet", y = "formula"),
	function(x, y, ...) {
		plot(x, formula=y, ...)
	})

