
setMethod("plot",
	signature = c(x = "MSImageSet", y = "missing"),
	function(x, formula = ~ mz,
		pixel = pixels(x, coord=coord),
		pixel.groups = pixelNames(x)[pixel],
		coord,
		plusminus,
		...,
		type = ifelse(centroided(x), 'h', 'l'))
	{
		if ( !missing(plusminus) && plusminus != 0 ) {
			newpixels <- spatial.neighbors(x, r=plusminus,
				indices=pixel, na.rm=TRUE)
			pixel.groups <- rep(pixel.groups,
				each=length(newpixels[[1]]))
			pixel <- unlist(newpixels)
		}
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
		feature.groups = featureNames(x)[feature],
		mz,
		plusminus,
		...)
	{
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
		callNextMethod(x,
			formula=formula,
			feature=feature,
			feature.groups=feature.groups,
			...)
	})
