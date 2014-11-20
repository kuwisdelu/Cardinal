
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
		mz <- fun(iData(object)[,pixel[1]], mz(object), ...)$t
		data <- pixelApply(object, function(s, ...) {
			sout <- fun(s, mz(object), ...)
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
		feature <- features(object, mz=mz)
		object@featureData <- object@featureData[feature,]
		object@pixelData <- object@pixelData[pixel,]
		object@imageData <- MSImageData(data=data,
			coord=coord(object@pixelData),
			storageMode=storageMode(imageData(object)),
			dimnames=list(
				featureNames(object@featureData),
				pixelNames(object@pixelData)))
		mz(object) <- mz
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
		reduceDimension(object, method=method, peaklist=ref, ...)
	})

setMethod("reduceDimension", signature = c(object = "MSImageSet", ref= "MSImageSet"),
	function(object, ref, method = "peaks", ...) {
		if ( !centroided(ref) )
			.stop("reduceDimension: 'ref' is not centroided. Run 'peakAlign' on it first.")
		prochistory(processingData(object)) <- .history()
		object <- reduceDimension(object, method=method, peaklist=mz(ref), ...)
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

reduceDimension.bin <- function(x, t, width=1, offset=0, fun=sum, ...) {
	limits <- seq(from=ceiling(min(t)), to=floor(max(t)), by=width) + offset
	if ( length(limits) > length(t) )
		.stop("reduceDimension.bin: 'width' is too wide.")
	idx <- sapply(limits, function(lim) bisection.seq(t, function(ti) ti - lim))
	xout <- bin(x, lbound=idx[-length(idx)], ubound=idx[-1], fun=fun)
	tout <- limits[-length(limits)] + width / 2
	list(x=xout, t=tout)
}

reduceDimension.resample <- function(x, t, step=1, offset=0, ...) {
	tout <- seq(from=ceiling(min(t)), to=floor(max(t)), by=step) + offset
	if ( length(tout) > length(t) )
		.stop("reduceDimension.resample: 'step' is too wide.")
	if ( offset < 0 ) {
		tout <- tout[-1]
	} else if ( offset > 0 ) {
		tout <- tout[-length(tout)]
	}
	xout <- interp1(x=t, y=x, xi=tout, method="linear", ...)
	list(x=xout, t=tout)
}

reduceDimension.peaks <- function(x, t, peaklist, type=c("height", "area"), ...) {
	type <- match.arg(type)
	if ( type == "height" ) {
		fun <- max
	} else if ( type == "area" ) {
		fun <- sum
	}
	if ( length(peaklist) > length(t) )
		.stop("reduceDimension.peaks: 'peaklist' is too long.")
	limits <- nearestLocalMaxima(-x, t, peaklist, ...)
	lbound <- sapply(limits$lbound,
		function(lim) bisection.seq(t, function(ti) ti - lim))
	ubound <- sapply(limits$ubound,
		function(lim) bisection.seq(t, function(ti) ti - lim))
	xout <- bin(x, lbound=lbound, ubound=ubound, fun=fun)
	list(x=xout, t=peaklist)
}

