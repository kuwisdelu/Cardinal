
#### Detect peaks in spectra ####
## ------------------------------

setMethod("peakPick", "MSImagingExperiment",
	function(object, method = c("simple", "adaptive"), ...)
	{
		fun <- peakPick_fun(peakPick.method2(method))
		object <- process(object, fun=fun, ...,
			label="peakPick", kind="pixel",
			postfun=peakPick_postfun,
			plotfun=peakPick_plotfun,
			delay=TRUE)
		object
	})

peakPick_plotfun <- function(s2, s1, ...,
	main="Peak picking", xlab="m/z", ylab="")
{
	mz <- mz(attr(s1, "mcols"))
	plot(mz, s1, main=main, xlab=xlab, ylab=ylab,
		col="gray", type='l', ...)
	noise <- attr(s2, "noise")
	if ( !is.null(noise) )
		lines(mz, noise, col="blue", lwd=0.5)
	s2mz <- s2[1:floor(length(s2) / 2)]
	s2i <- s2[floor(1 + (length(s2) / 2)):length(s2)]
	lines(s2mz, s2i, col="red", type='h')
}

peakPick_fun <- function(f) {
	fun <- function(x, ...) {
		i <- f(x, ...)
		mz <- mz(attr(x, "mcols"))
		y <- as.numeric(c(mz[i], x[i]))
		attributes(y) <- attributes(i)
		y
	}
	fun
}

peakPick_postfun <- function(object, ans, ...) {
	numPeaks <- floor(lengths(ans) / 2)
	.message("detected ~", round(mean(numPeaks)), " peaks per spectrum ",
		"(min = ", min(numPeaks), ", max = ", max(numPeaks), ")")
	tol <- c(absolute=min(abs(diff(mz(object)))) / 2)
	data <- .collect_keyval_pairs(ans)
	data <- sparse_mat(data, keys=mz(object),
		nrow=nrow(object), ncol=ncol(object),
		tolerance=tol, combiner="sum")
	imageData(object) <- MSProcessedImagingSpectraList(data)
	object <- as(object, "MSProcessedImagingExperiment")
	object
}

peakPick.method2 <- function(method) {
	if ( is.character(method) ) {
		method <- match.method(method, c("simple", "adaptive"))
		switch(method,
			simple = peakPick.simple2,
			adaptive = peakPick.adaptive2,
			match.fun(method))
	} else {
		match.fun(method)
	}
}

peakPick.simple2 <- function(x, ...) {
	result <- peakPick.simple(x, ...)
	peaks <- which(result$peaks)
	attr(peaks, "noise") <- result$noise
	peaks
}

peakPick.adaptive2 <- function(x, ...) {
	result <- peakPick.adaptive(x, ...)
	peaks <- which(result$peaks)
	attr(peaks, "noise") <- result$noise
	peaks
}

