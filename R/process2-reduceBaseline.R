
#### Estimate and subtract baseline methods ####
## ---------------------------------------------

setMethod("reduceBaseline", "MSImagingExperiment",
	function(object, method = "median", ...)
	{
		fun <- reduceBaseline.method2(method)
		object <- process(object, fun=fun, ...,
			label="reduceBaseline", kind="pixel",
			plotfun=reduceBaseline_plotfun,
			delay=TRUE)
		object
	})

reduceBaseline_plotfun <- function(s2, s1, ...,
	main="Baseline reduction", xlab="m/z", ylab="")
{
	mz <- mz(attr(s1, "mcols"))
	plot(mz, s1, main=main, xlab=xlab, ylab=ylab,
		col="gray", type='l', ...)
	lines(mz, s1 - s2, col="green")
	lines(mz, s2, lwd=0.5)
}

reduceBaseline.method2 <- function(method) {
	if ( is.character(method) ) {
		method <- match.method(method, c("median"))
		switch(method,
			median = reduceBaseline.median2,
			match.fun(method))
	} else {
		match.fun(method)
	}
}

reduceBaseline.median2 <- reduceBaseline.median
