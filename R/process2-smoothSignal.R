
#### Smooth spectra ####
## ---------------------

setMethod("smoothSignal", "MSImagingExperiment",
	function(object, method = c("gaussian", "sgolay", "ma"), ...)
	{
		fun <- smoothSignal.method2(method)
		object <- process(object, fun=fun, ...,
			label="smoothSignal", kind="pixel",
			plotfun=smoothSignal_plotfun,
			delay=TRUE)
		object
	})

smoothSignal_plotfun <- function(s2, s1, ...,
	main="Smoothing", xlab="m/z", ylab="")
{
	mz <- mz(attr(s1, "mcols"))
	plot(mz, s1, main=main, xlab=xlab, ylab=ylab,
		col="gray", type='l', ...)
	lines(mz, s2, lwd=0.5)
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

smoothSignal.ma2 <- smoothSignal.ma

smoothSignal.gaussian2 <- smoothSignal.gaussian

smoothSignal.kaiser2 <- smoothSignal.kaiser

smoothSignal.sgolay2 <- smoothSignal.sgolay
