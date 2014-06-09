
setMethod("plot",
	signature = c(x = "MSImageSet", y = "missing"),
	function(x, formula = ~ mz,
		pixel = pixels(x, coord=coord),
		coord,
		plusminus,
		...,
		xlab,
		sub)
	{
		model <- .parseFormula(formula)
		if ( missing(xlab) ) {
			if ( names(model$right) == "mz" ) {
				xlab <- expression(italic(m/z))
			} else {
				xlab <- names(model$right)
			}
		}
		if ( !missing(coord) ) {
			if ( length(pixel) > 1 ) {
				.warning("pixel has length > 1 and only the first element will be used")
				pixel <- pixel[[1]]
			}
			coord <- coord(x)[pixel[[1]],]
			sub <- .formatCoord(coord(x)[pixel[[1]],])
			if ( !missing(plusminus) ) {
				newcoord <- mapply(function(xyz, maxidx) {
					max(xyz[[1]]-plusminus, 1):min(xyz[[1]]+plusminus, maxidx)
				}, coord, sapply(coord(x), max))
				coord <- expand.grid(newcoord)
				pixel <- pixels(x, coord=coord)
			}
		} else if ( missing(sub) ) {
			coord <- NULL
			if ( length(pixel) > 1 ) {
				sub <- ""
			} else {
				sub <- .formatCoord(coord(x)[pixel[[1]],])
			}
		}
		callNextMethod(x,
			formula=formula,
			pixel=pixel,
			...,
			xlab=xlab,
			sub=sub)
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
				feature <- feature[[1]]
			}
			mz <- mz(x)[[feature]]
			sub <- .formatMZ(mz(x)[[feature]])
			if ( !missing(plusminus) ) {
				plus <- feature
				while ( mz(x)[[plus]] - mz < plusminus && plus < dim(x)[[1]] )
					plus <- plus + 1
				minus <- feature
				while ( mz - mz(x)[[minus]] < plusminus && minus > 1 )
					minus <- minus - 1
				feature <- minus:plus
			}
		} else if ( missing(sub) ) {
			mz <- NULL
			if ( length(feature) > 1 ) {
				sub <- ""
			} else {
				sub <- .formatMZ(mz(x)[[feature]])
			}
		}
		callNextMethod(x,
			formula=formula,
			feature=feature,
			...,
			sub=sub)
	})

