
#### Dimension reduction methods ####
## ----------------------------------

setMethod("reduceDimension", signature = c(object = "MSImageSet", ref = "missing"),
	function(object, method = c("bin", "resample"),
		...,
		pixel = pixels(object),
		plot = FALSE)
	{
		fun <- reduceDimension.method(method)
		prochistory(processingData(object)) <- .history()
		.message("reduceDimension: Using method = ", match.method(method))
		.time.start()
		mzout <- fun(numeric(nrow(object)), mz(object), ...)$t
		data <- pixelApply(object, function(s, ...) {
			sout <- fun(s, mz(object), mzout, ...)
			if ( plot ) {
				wrap(plot(object, pixel=.Index, col="gray", ...),
					..., signature=fun)
				wrap(points(sout$t, sout$x, col="red", pch=20, ...),
					..., signature=fun)
				wrap(lines(sout$t, sout$x, col="red", type='h', lwd=0.5, ...),
					..., signature=fun)
			}
			sout$x
		}, .pixel=pixel, ..., .use.names=FALSE, .simplify=TRUE)
		feature <- features(object, mz=mzout)
		object@featureData <- object@featureData[feature,]
		object@pixelData <- object@pixelData[pixel,]
		object@imageData <- MSImageData(data=data,
			coord=coord(object@pixelData),
			storageMode=storageMode(imageData(object)),
			dimnames=list(
				featureNames(object@featureData),
				pixelNames(object@pixelData)))
		mz(object) <- mzout
		if ( match.method(method) == "peaks" ) {
			spectrumRepresentation(processingData(object)) <- "centroid"
			centroided(processingData(object)) <- TRUE
		}
		.message("reduceDimension: Done")
		.time.stop()
		object
	})

setMethod("reduceDimension", signature = c(object = "MSImageSet", ref = "numeric"),
	function(object, ref, method = "peaks", ...) {
		if ( min(ref) < min(mz(object)) || max(ref) > max(mz(object)) )
			.stop("reduceDimension: 'ref' contains m/z values outside of mass range.")
		prochistory(processingData(object)) <- .history()
		reduceDimension(object, method=method, tout=ref, ...)
	})

setMethod("reduceDimension", signature = c(object = "MSImageSet", ref= "MSImageSet"),
	function(object, ref, method = "peaks", ...) {
		if ( !centroided(ref) )
			.stop("reduceDimension: 'ref' is not centroided. Run 'peakAlign' on it first.")
		prochistory(processingData(object)) <- .history()
		object <- reduceDimension(object, method=method, tout=mz(ref), ...)
		peakPicking(processingData(object)) <- peakPicking(processingData(ref))
		object
	})

reduceDimension.method <- function(method) {
	if ( is.character(method) ) {
		method <- match.method(method, c("bin", "resample", "peaks"))
		method <- switch(method,
			bin = reduceDimension.bin,
			resample = reduceDimension.resample,
			peaks = reduceDimension.peaks,
			match.fun(method))
	}
	match.fun(method)
}

reduceDimension.bin <- function(x, t, tout, width=200, offset=0, units=c("ppm", "mz"), fun=sum, ...) {
	units <- match.arg(units)
	if ( missing(tout) ) {
		if ( units == "ppm" ) {
			tout <- seq.ppm(from=offset + floor(min(t)), to=ceiling(max(t)), ppm=width)
			width <- width * 1e-6 * tout
		} else {
			tout <- seq(from=offset + floor(min(t)), to=ceiling(max(t)), by=width)
			width <- rep(width, length(tout))
		}
	}
	if ( length(tout) > length(t) )
		.stop("reduceDimension.bin: 'width' is too small.")
	xout <- bin(x, t, lbound=tout - width / 2, ubound=tout + width / 2, fun=fun)
	list(x=xout, t=tout)
}

reduceDimension.resample <- function(x, t, tout, step=1, offset=0, ...) {
	if ( missing(tout) )
		tout <- seq(from=ceiling(min(t)), to=floor(max(t)), by=step) + offset
	if ( length(tout) > length(t) )
		.stop("reduceDimension.resample: 'step' is too small.")
	if ( offset < 0 ) {
		tout <- tout[-1]
	} else if ( offset > 0 ) {
		tout <- tout[-length(tout)]
	}
	xout <- interp1(x=t, y=x, xi=tout, method="linear", ...)
	list(x=xout, t=tout)
}

reduceDimension.peaks <- function(x, t, tout, type=c("height", "area"), ...) {
	if ( missing(tout) )
		.stop("reduceDimension.peaks: 'tout' required.")
	type <- match.arg(type)
	if ( type == "height" ) {
		fun <- max
	} else if ( type == "area" ) {
		fun <- sum
	}
	if ( length(tout) > length(t) )
		.stop("reduceDimension.peaks: 'tout' is too long.")
	limits <- nearestLocalMaxima(-x, t, tout)
	xout <- bin(x, t, lbound=limits$lbound, ubound=limits$ubound, fun=fun)
	list(x=xout, t=tout)
}

