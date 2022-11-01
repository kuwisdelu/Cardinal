
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
			delay=getCardinalDelayProc())
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
	data <- sparse_mat(index=data$keys, data=data$values,
		domain=mz(object), nrow=nrow(object), ncol=ncol(object),
		tolerance=tol, sampler="max")
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

peakPick.simple <- function(x, SNR=6, window=5, blocks=100, ...) {
	noise <- .estimateNoiseSimple(x, blocks=blocks)
	maxs <- which(locmax(x, width=window))
	peaks <- intersect(maxs, which(x / noise >= SNR))
	list(peaks=peaks, noise=noise)
}

peakPick.adaptive <- function(x, SNR=6, window=5, blocks=100, spar=1, ...) {
	noise <- .estimateNoiseAdaptive(x, blocks=blocks, spar=spar)
	maxs <- which(locmax(x, width=window))
	peaks <- intersect(maxs, which(x / noise >= SNR))
	list(peaks=peaks, noise=noise)
}

peakPick.limpic <- function(x, SNR=6, window=5, blocks=100, thresh=0.75, ...) {
	t <- seq_along(x)
	xint <- split_blocks(x, blocks=blocks)
	# identify flat regions of spectrum
	kurt <- sapply(xint, kurtosis) - 3
	kurt[is.nan(kurt)] <- -Inf
	means <- sapply(xint, mean)
	is.flat <- kurt < 1 & means < mean(x)
	# estimate noise
	noiseval <- sapply(xint, sd)[is.flat]
	noiseval <- c(median(noiseval, na.rm=TRUE), noiseval, median(noiseval, na.rm=TRUE))
	noiseidx <- floor(seq(from=blocks/2, to=length(x) - blocks/2,
		length.out=length(xint)))[is.flat]
	noiseidx <- c(1, noiseidx, length(x))
	noise <- interp1(x=t[noiseidx], y=noiseval, xi=t, method="linear")
	# find local maxima
	halfWindow <- floor(window / 2)
	maxs <- which(locmax(x, width=window))
	peaks <- intersect(maxs, which(x / noise >= SNR))
	# eliminate smooth peaks
	cutoff <- ceiling(halfWindow / 2) * quantile(abs(diff(x)), thresh) / 2
	too.flat <- sapply(peaks, function(i) {
		if ( (i - halfWindow) < 0 || (i + halfWindow) > length(x) ) {
			TRUE	
		} else {
			ratio <- x[i] - 0.5 * (x[i - halfWindow] + x[i + halfWindow])
			ratio < cutoff
		}
	})
	# return peak list
	peaks <- setdiff(peaks, too.flat)
	list(peaks=peaks, noise=noise)
}

peakPick.mad <- function(x, SNR=6, window=5, blocks=1, fun=mean, tform=diff, ...) {
	noise <- .estimateNoiseMAD(x, blocks=blocks, fun=fun, tform=tform)
	maxs <- which(locmax(x, width=window))
	peaks <- intersect(maxs, which(x / noise >= SNR))
	attr(peaks, "noise") <- noise
	peaks
}

peakPick.simple2 <- function(x, ...) {
	result <- peakPick.simple(x, ...)
	peaks <- result$peaks
	attr(peaks, "noise") <- result$noise
	peaks
}

peakPick.adaptive2 <- function(x, ...) {
	result <- peakPick.adaptive(x, ...)
	peaks <- result$peaks
	attr(peaks, "noise") <- result$noise
	peaks
}

# Estimate noise in a signal

.estimateNoiseSimple <- function(x, blocks=100) {
	t <- seq_along(x)
	xint <- split_blocks(x, blocks=blocks)
	kurt <- sapply(xint, kurtosis) - 3
	if ( all(kurt >= 1, na.rm=TRUE) ) {
		noise <- min(sapply(xint, sd), na.rm=TRUE)
		noise <- rep(noise, length(x))
		if ( anyNA(kurt) )
			.warning("kurtosis could not be estimated; ",
				"try method = 'mad' instead")
	} else {
		kurt[is.na(kurt)] <- Inf
		noise <- mean(sapply(xint, sd)[kurt < 1], na.rm=TRUE)
		noise <- rep(noise, length(x))
	}
	noise
}

.estimateNoiseAdaptive <- function(x, blocks=100, spar=1) {
	t <- seq_along(x)
	tint <- split_blocks(t, blocks=blocks)
	xint <- split_blocks(x, blocks=blocks)
	kurt <- sapply(xint, kurtosis) - 3
	if ( all(kurt >= 1, na.rm=TRUE) ) {
		noise <- min(sapply(xint, sd), na.rm=TRUE)
		noise <- rep(noise, length(x))
		if ( anyNA(kurt) )
			.warning("kurtosis could not be estimated; ",
				"try method = 'mad' instead")
	} else {
		kurt[is.na(kurt)] <- Inf
		noiseval <- sapply(xint, sd)[kurt < 1]
		noiseidx <- sapply(tint, mean)[kurt < 1]
		noise <- interp1(noiseidx, noiseval, xi=t, method="linear",
			extrap=mean(noiseval, na.rm=TRUE))
		noise <- smooth.spline(x=t, y=noise, spar=1)$y
	}
	noise
}

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
