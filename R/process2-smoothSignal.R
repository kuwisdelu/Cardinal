
#### Smooth spectra ####
## ---------------------

setMethod("smooth", "SparseImagingExperiment",
	function(x, ...)
	{
		smoothSignal(x, ...)
	})

setMethod("smoothSignal", "SparseImagingExperiment",
	function(object, method = c("gaussian", "sgolay", "ma"), ...)
	{
		dotargs <- list(...)
		fun <- smoothSignal.method2(method)
		object <- process(object, fun=fun,
			label="smoothSignal", kind="pixel",
			plotfun=smoothSignal_plotfun,
			moreargs=dotargs,
			delay=getCardinalDelayProc())
		object
	})

smoothSignal.method2 <- function(method) {
	if ( is.character(method) ) {
		method <- match.method(method, c("gaussian", "sgolay", "ma"))
		fun <- switch(method,
			gaussian = smoothSignal.gaussian2,
			sgolay = smoothSignal.sgolay2,
			ma = smoothSignal.ma2,
			match.fun(method))
	} else {
		fun <- match.fun(method)
	}
	attr(fun, "method") <- deparse(method)
	fun
}

smoothSignal_plotfun <- function(s2, s1, ...,
	main="Smoothing", xlab="", ylab="")
{
	mcols <- attr(s1, "mcols")
	if ( is(mcols, "MassDataFrame") ) {
		x <- mz(mcols)
		if ( missing(xlab) )
			xlab <- "m/z"
	} else {
		x <- seq_along(s2)
	}
	plot(x, s1, main=main, xlab=xlab, ylab=ylab,
		col="gray", type='l', ...)
	lines(x, s2, lwd=0.5)
}

smoothSignal.ma <- function(x, coef=rep(1, window + 1 - window %% 2), window=5, ...) {
	coef <- coef / sum(coef)
	window <- length(coef)
	halfWindow <- floor(window / 2)
	xpad <- c(rep(x[1], halfWindow), x, rep(x[length(x)], halfWindow))
	convolve(xpad, coef, type="filter")
}

smoothSignal.gaussian <- function(x, sd=window/4, window=5, ...) {
	halfWindow <- floor(window / 2)
	coef <- dnorm((-halfWindow):halfWindow, sd=sd)
	smoothSignal.ma(x, coef=coef, ...)
}

smoothSignal.kaiser <- function(x, beta=1, window=5, ...) {
	coef <- kaiser(n=window + 1 - window %% 2, beta=beta)
	smoothSignal.ma(x, coef=coef, ...)
}

smoothSignal.sgolay <- function(x, order=3, window=order + 3 - order %% 2, ...) {
	window <- window + 1 - window %% 2
	sgolayfilt(x, p=order, n=window)
}

smoothSignal.gaussian2 <- smoothSignal.gaussian

smoothSignal.sgolay2 <- smoothSignal.sgolay

smoothSignal.ma2 <- smoothSignal.ma


