
#### Signal smoothing methods ####
## -------------------------------

setMethod("smoothSignal", "MSImagingExperiment",
	function(object, method = c("gaussian", "sgolay", "ma"), ...)
	{
		fun <- smoothSignal.method2(method)
		e <- new.env(parent=getNamespace("Cardinal"))
		e$mz <- mz(object)
		plotfun <- function(s1, s2, ...,
			main="Smoothing", xlab="m/z", ylab="")
		{
			plot(mz, s2, main=main, xlab=xlab, ylab=ylab,
				col="gray", type='l', ...)
			lines(mz, s1, lwd=0.5)
		}
		environment(plotfun) <- e
		object <- process(object, fun=fun, ...,
			label="smoothSignal", kind="pixel",
			plotfun=plotfun, delay=TRUE)
		object
	})

setMethod("smoothSignal", "MSImageSet",
	function(object, method = c("gaussian", "sgolay", "ma"),
		...,
		pixel = pixels(object),
		plot = FALSE)
	{
		if ( centroided(object) )
			.stop("smoothSignal: Data already centroided. Smoothing will not be performed.")
		fun <- smoothSignal.method(method)
		prochistory(processingData(object)) <- .history()
		.message("smoothSignal: Using method = ", match.method(method))
		.time.start()
		data <- pixelApply(object, function(s, ...) {
			smoothSignal.do(s, object, .Index, fun, plot, ...)
		}, .pixel=pixel, ..., .use.names=FALSE, .simplify=TRUE)
		object@imageData <- MSImageData(data=data,
			coord=coord(object)[pixel,],
			storageMode=storageMode(object@imageData),
			dimnames=list(featureNames(object), pixelNames(object)[pixel]))
		object@pixelData <- object@pixelData[pixel,]
		smoothing(processingData(object)) <- match.method(method)
		.message("smoothSignal: Done")
		.time.stop()
		object
	})

smoothSignal.do <- function(s, object, pixel, f, plot, ...) {
	sout <- f(s, ...)
	if ( plot ) {
		wrap(plot(object, s ~ mz, pixel=pixel, col="gray",
			ylab="Intensity", strip=FALSE, ...),
			..., signature=f)
		wrap(lines(mz(object), sout, lwd=0.5, ...),
			..., signature=f)
	}
	sout
}

smoothSignal.method <- function(method, name.only=FALSE) {
	if ( is.character(method) || is.null(method) ) {
		options <- c("gaussian", "sgolay", "ma")
		method <- match.method(method, options)
		if ( name.only )
			return(method)
		method <- switch(method,
			gaussian = smoothSignal.gaussian,
			sgolay = smoothSignal.sgolay,
			ma = smoothSignal.ma,
			match.fun(method))
	}
	match.fun(method)
}

smoothSignal.method2 <- function(method) {
	if ( is.character(method) ) {
		method <- match.method(method, c("gaussian", "sgolay", "ma"))
		switch(method,
			gaussian = smoothSignal.gaussian2,
			sgolay = smoothSignal.sgolay2,
			ma = smoothSignal.ma2,
			match.fun(method))
	} else {
		match.fun(method)
	}
}

smoothSignal.ma <- function(x, coef=rep(1, window + 1 - window %% 2), window=5, ...) {
	coef <- coef / sum(coef)
	window <- length(coef)
	halfWindow <- floor(window / 2)
	xpad <- c(rep(x[1], halfWindow), x, rep(x[length(x)], halfWindow))
	filter(xpad, filter=coef)[(halfWindow + 1):(length(xpad) - halfWindow)]
}

smoothSignal.ma2 <- smoothSignal.ma

smoothSignal.gaussian <- function(x, sd=window/4, window=5, ...) {
	halfWindow <- floor(window / 2)
	coef <- dnorm((-halfWindow):halfWindow, sd=sd)
	smoothSignal.ma(x, coef=coef, ...)
}

smoothSignal.gaussian2 <- smoothSignal.gaussian

smoothSignal.kaiser <- function(x, beta=1, window=5, ...) {
	coef <- kaiser(n=window + 1 - window %% 2, beta=beta)
	smoothSignal.ma(x, coef=coef, ...)
}

smoothSignal.kaiser2 <- smoothSignal.kaiser

smoothSignal.sgolay <- function(x, order=3, window=order + 3 - order %% 2, ...) {
	window <- window + 1 - window %% 2
	sgolayfilt(x, p=order, n=window)
}

smoothSignal.sgolay2 <- smoothSignal.sgolay

