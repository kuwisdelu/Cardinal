
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
		if ( !missing(coord) && missing(pixel.groups) ) {
			 pixel.groups <- pixelNames(x)[pixel]
			 if ( !missing(plusminus) && plusminus != 0 ) {
				newpixels <- spatial.neighbors(x, r=plusminus,
					indices=pixel, na.rm=TRUE)
				pixel.groups <- rep(pixel.groups,
					each=length(newpixels[[1]]))
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

setMethod("image",
	signature = c(x = "MSImageSet"),
	function(x, formula = ~ x * y,
		feature = features(x, mz=mz),
		feature.groups,
		mz,
		plusminus,
		...)
	{
		if ( !missing(mz) && missing(feature.groups) ) {
			feature.groups <- featureNames(x)[feature]
			if ( !missing(plusminus) && plusminus != 0 ) {
				newfeatures <- lapply(mz, function(mzi) {
					seq(from=features(x, mz=mzi-plusminus),
						to=features(x, mz=mzi+plusminus),
						by=sign(plusminus))
				})
				feature.groups <- unlist(mapply(function(group, feature) {
					rep(group, length(feature))
				}, feature.groups, newfeatures))
				feature <- unlist(newfeatures)
			}
		} else if ( missing(feature.groups) ) {
			feature.groups <- NULL
		}
		if ( missing(feature) && missing(mz) && missing(formula) )
			.stop("image: 'feature' or 'mz' must be specified")
		callNextMethod(x,
			formula=formula,
			feature=feature,
			feature.groups=feature.groups,
			...)
	})
