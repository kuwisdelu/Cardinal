
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
			delay=getOption("Cardinal.delay"))
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

smoothSignal.gaussian2 <- smoothSignal.gaussian

smoothSignal.sgolay2 <- smoothSignal.sgolay

smoothSignal.ma2 <- smoothSignal.ma


