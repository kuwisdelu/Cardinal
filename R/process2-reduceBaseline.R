
#### Estimate and subtract baseline methods ####
## ---------------------------------------------

setMethod("reduceBaseline", "SparseImagingExperiment",
	function(object, method = c("locmin", "median"), ...)
	{
		dotargs <- list(...)
		fun <- reduceBaseline.method2(method)
		object <- process(object, fun=fun,
			label="reduceBaseline", kind="pixel",
			moreargs=dotargs,
			plotfun=reduceBaseline_plotfun,
			delay=getCardinalDelayProc())
		object
	})

reduceBaseline.method2 <- function(method) {
	if ( is.character(method) ) {
		method <- match.method(method,
			c("locmin", "median"))
		fun <- switch(method,
			locmin = reduceBaseline.locmin,
			median = reduceBaseline.median2,
			match.fun(method))
	} else {
		fun <- match.fun(method)
	}
	attr(fun, "method") <- deparse(method)
	fun
}

reduceBaseline_plotfun <- function(s2, s1, ...,
	main="Baseline reduction", xlab="", ylab="")
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
	lines(x, s1 - s2, col="green")
	lines(x, s2, lwd=0.5)
}

reduceBaseline.median <- function(x, blocks=500, fun=median, spar=1, ...) {
	xint <- split_blocks(x, blocks=blocks)
	baseval <- sapply(xint, fun)
	baseidx <- sapply(xint, function(xi) which.min(abs(fun(xi) - xi)))
	baseidx <- baseidx + c(0, cumsum(sapply(xint, length))[-length(xint)])
	if ( diff(range(baseval))==0 )
		return(rep(0, length(x)))
	if ( !is.na(spar) ) {
		cutoff <- smooth.spline(x=baseidx, y=baseval, spar=spar)$y
		keep <- which(baseval <= cutoff)
		baseidx <- baseidx[keep]
		baseval <- baseval[keep]
	}
	baseval[c(1,length(baseval))] <- c(fun(xint[[1]]), fun(xint[[length(xint)]]))
	baseidx[c(1,length(baseidx))] <- c(1, length(x))
	baseline <- interp1(x=baseidx, y=baseval, xi=seq_along(x), method="linear")
	pmax(x - baseline, 0)
}

reduceBaseline.median2 <- reduceBaseline.median

reduceBaseline.locmin <- function(x, window = 5, ...) {
	baseidx <- which(locmax(-x, width=window))
	if ( length(baseidx) != 0L ) {
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
		x <- pmax(x - baseline, 0)	
	}
	x
}


