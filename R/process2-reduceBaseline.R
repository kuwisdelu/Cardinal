
#### Estimate and subtract baseline methods ####
## ---------------------------------------------

setMethod("reduceBaseline", "MSImagingExperiment",
	function(object, method = c("median", "locmin"), ...)
	{
		fun <- reduceBaseline.method2(method)
		object <- process(object, fun=fun, ...,
			label="reduceBaseline", kind="pixel",
			plotfun=reduceBaseline_plotfun,
			delay=TRUE)
		object
	})

reduceBaseline.method2 <- function(method) {
	if ( is.character(method) ) {
		method <- match.method(method,
			c("median", "locmin"))
		switch(method,
			median = reduceBaseline.median2,
			locmin = reduceBaseline.locmin,
			match.fun(method))
	} else {
		match.fun(method)
	}
}

reduceBaseline_plotfun <- function(s2, s1, ...,
	main="Baseline reduction", xlab="m/z", ylab="")
{
	mz <- mz(attr(s1, "mcols"))
	plot(mz, s1, main=main, xlab=xlab, ylab=ylab,
		col="gray", type='l', ...)
	lines(mz, s1 - s2, col="green")
	lines(mz, s2, lwd=0.5)
}

reduceBaseline.median2 <- reduceBaseline.median

reduceBaseline.locmin <- function(x, window = 5, ...) {
	baseidx <- which(localMaximaLogical(-x))
	baseval <- x[baseidx]
	if ( baseidx[1L] != 1L ) {
		baseval <- c(baseval[1L], baseval)
		baseidx <- c(1L, baseidx)
	}
	if ( baseidx[length(baseidx)] != length(x) ) {
		baseval <- c(baseval, baseval[length(baseval)])
		baseidx <- c(baseidx, length(x))
	}
	baseline <- interp1(x=baseidx, y=baseval, xi=seq_along(x), method="linear")
	pmax(x - baseline, 0)
}


