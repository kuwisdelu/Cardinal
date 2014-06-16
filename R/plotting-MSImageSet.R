
setMethod("plot",
	signature = c(x = "MSImageSet", y = "missing"),
	function(x, formula = ~ mz,
		pixel = pixels(x, coord=coord),
		coord,
		plusminus,
		...,
		type = ifelse(centroided(x), 'h', 'l'),
		sub)
	{
		model <- .parseFormula(formula)
		if ( !missing(coord) ) {
			if ( length(pixel) > 1 ) {
				.warning("pixel has length > 1 and only the first element will be used")
				pixel <- pixel[1]
			}
			coord <- coord(x)[pixel[1],]
			sub <- .format.list(coord(x)[pixel[1],])
			if ( !missing(plusminus) ) {
				if ( length(plusminus) != length(coord) )
					plusminus <- rep(plusminus, length.out=length(coord))
				newcoord <- mapply(function(xyz, pm, maxidx) {
					max(xyz[1]-pm, 1):min(xyz[1]+pm, maxidx)
				}, coord, plusminus, sapply(coord(x), max), SIMPLIFY=FALSE)
				coord <- expand.grid(newcoord)
				pixel <- pixels(x, coord=coord)
			}
		} else if ( missing(sub) ) {
			coord <- NULL
			if ( length(pixel) > 1 ) {
				sub <- ""
			} else {
				sub <- .format.list(coord(x)[pixel,])
			}
		}
		callNextMethod(x,
			formula=formula,
			pixel=pixel,
			...,
			sub=sub,
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
		mz,
		plusminus,
		...,
		sub)
	{
		if ( !missing(mz) ) {
			if ( length(feature) > 1 ) {
				.warning("feature has length > 1 and only the first element will be used")
				feature <- feature[1]
			}
			mz <- mz(x)[feature]
			sub <- .format.mz(mz(x)[feature])
			if ( !missing(plusminus) ) {
				plus <- feature
				while ( mz(x)[plus] - mz < plusminus && plus < dim(x)[1] )
					plus <- plus + 1
				minus <- feature
				while ( mz - mz(x)[minus] < plusminus && minus > 1 )
					minus <- minus - 1
				feature <- minus:plus
			}
		} else if ( missing(sub) ) {
			mz <- NULL
			if ( length(feature) > 1 ) {
				sub <- ""
			} else {
				sub <- .format.mz(mz(x)[feature])
			}
		}
		callNextMethod(x,
			formula=formula,
			feature=feature,
			...,
			sub=sub)
	})

