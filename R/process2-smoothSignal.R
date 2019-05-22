
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
		fun <- smoothSignal.method2(method)
		object <- process(object, fun=fun,
			label="smoothSignal", kind="pixel",
			plotfun=smoothSignal_plotfun,
			moreargs=list(...),
			delay=getOption("Cardinal.delay"))
		object
	})

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

smoothSignal.ma2 <- smoothSignal.ma

smoothSignal.gaussian2 <- smoothSignal.gaussian

smoothSignal.kaiser2 <- smoothSignal.kaiser

smoothSignal.sgolay2 <- smoothSignal.sgolay
