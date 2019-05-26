
#### Detect peaks in spectra ####
## ------------------------------

setMethod("peakPick", "MSImagingExperiment",
	function(object, method = c("mad", "simple", "adaptive"), ...)
	{
		dotargs <- list(...)
		fun <- peakPick.method2(method)
		object <- process(object, fun=peakPick_fun(fun),
			label="peakPick", kind="pixel",
			postfun=peakPick_postfun,
			plotfun=peakPick_plotfun,
			moreargs=dotargs,
			delay=getOption("Cardinal.delay"))
		object
	})

peakPick.method2 <- function(method) {
	if ( is.character(method) ) {
		method <- match.method(method,
			c("mad", "simple", "adaptive"))
		fun <- switch(method,
			mad = peakPick.mad,
			simple = peakPick.simple2,
			adaptive = peakPick.adaptive2,
			match.fun(method))
	} else {
		fun <- match.fun(method)
	}
	attr(fun, "method") <- deparse(method)
	fun
}

peakPick_fun <- function(f) {
	fun <- function(x, ...) {
		i <- f(x, ...)
		mz <- mz(attr(x, "mcols"))
		y <- as.numeric(c(mz[i], x[i]))
		attributes(y) <- attributes(i)
		y
	}
	attr(fun, "method") <- attr(f, "method")
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

peakPick.mad <- function(x, SNR=6, window=5, blocks=1, fun=mean, tform=diff, ...) {
	noise <- .estimateNoiseMAD(x, blocks=blocks, fun=fun, tform=tform)
	is.max <- localMaximaLogical(x, window=window)
	peaks <- is.max & (x / noise) >= SNR
	peaks[is.na(peaks)] <- FALSE
	peaks <- which(peaks)
	attr(peaks, "noise") <- noise
	peaks
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

# Estimate noise in a signal

.estimateNoiseMAD <- function(x, blocks=1, fun=mean, tform=diff) {
	mad <- function(y, na.rm) {
		center <- fun(tform(y), na.rm=na.rm)
		fun(abs(y - center), na.rm=na.rm)
	}
	if ( blocks > 1 ) {
		t <- seq_along(x)
		xint <- split_blocks(x, blocks=blocks)
		tint <- split_blocks(t, blocks=blocks)
		noiseval <- sapply(xint, mad, na.rm=TRUE)
		noiseidx <- sapply(tint, mean, na.rm=TRUE)
		noise <- interp1(noiseidx, noiseval, xi=t, method="linear",
			extrap=fun(noiseval, na.rm=TRUE))
		noise <- supsmu(x=t, y=noise)$y
	} else {
		noise <- mad(x, na.rm=TRUE)
		noise <- rep(noise, length(x))
	}
	noise
}
