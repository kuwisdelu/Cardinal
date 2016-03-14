
#### Peak picking methods ####
## ---------------------------

setMethod("peakPick", "MSImageSet",
	function(object, method = c("simple", "adaptive", "limpic"),
		...,
		pixel = pixels(object),
		plot = FALSE)
	{
		if ( centroided(object) )
			.stop("peakPick: Data already centroided. Peak picking will not be performed.")
		fun <- peakPick.method(method)
		prochistory(processingData(object)) <- .history()
		.message("peakPeak: Using method = ", match.method(method))
		.time.start()
		peaks <- pixelApply(object, function(s, ...) {
			peakPick.do(s, object, .Index, fun, plot, ...)
		}, .pixel=pixel, ..., .use.names=FALSE, .simplify=FALSE)
		peakData <- lapply(seq_along(pixel), function(i) {
			intensityArray <- iData(object)[peaks[[i]], pixel[i]]
			names(intensityArray) <- featureNames(object)[peaks[[i]]]
			intensityArray
		})
		mzData <- lapply(seq_along(pixel), function(i) {
			mzArray <- mz(object)[peaks[[i]]]
			names(mzArray) <- featureNames(object)[peaks[[i]]]
			mzArray
		})
		object <- object[,pixel]
		peakData <- new("Hashmat", data=peakData, keys=featureNames(object),
			dim=c(length(features(object)), length(peakData)))
		mzData <- new("Hashmat", data=mzData, keys=featureNames(object),
			dim=c(length(features(object)), length(mzData)))
		peakData(imageData(object)) <- peakData
		mzData(imageData(object)) <- mzData
		peakPicking(processingData(object)) <- match.method(method)
		.message("peakPick: Done.")
		.time.stop()
		object
	})

peakPick.do <- function(s, object, pixel, f, plot, ...) {
	pout <- f(s, ...)
	if ( plot ) {
		wrap(plot(object, s ~ mz, pixel=pixel, col="gray",
			ylab="Intensity", strip=FALSE, ...),
			..., signature=f)
		wrap(lines(mz(object), pout$noise, col="blue", ...),
			..., signature=f)
		wrap(lines(mz(object)[pout$peaks], s[pout$peaks], col="red", type='h', ...),
			..., signature=f)
	}
	pout$peaks
}

peakPick.method <- function(method, name.only=FALSE) {
	if ( is.character(method) || is.null(method) ) {
		options <- c("simple", "adaptive", "limpic")
		method <- match.method(method, options)
		if ( name.only )
			return(method)
		method <- switch(method,
			simple = peakPick.simple,
			adaptive = peakPick.adaptive,
			limpic = peakPick.limpic,
			match.fun(method))
	}
	match.fun(method)
}

peakPick.simple <- function(x, SNR=6, window=5, blocks=100, ...) {
	t <- seq_along(x)
	xint <- blocks(x, blocks=blocks)
	kurt <- sapply(xint, kurtosis) - 3
	noise <- mean(sapply(xint, sd)[kurt < 1], na.rm=TRUE)
	noise <- rep(noise, length(x))
	is.max <- localMaximaLogical(x, window=window)
	peaks <- is.max & (x / noise) >= SNR
	peaks[is.na(peaks)] <- FALSE
	list(peaks=peaks, noise=noise)
}

peakPick.adaptive <- function(x, SNR=6, window=5, blocks=100, spar=1, ...) {
	t <- seq_along(x)
	xint <- blocks(x, blocks=blocks)
	xlen <- sapply(xint, length)
	noise <- sapply(xint, sd)
	noise <- unlist(mapply(function(ns, ln) rep(ns, ln), noise, xlen))
	cutoff <- smooth.spline(x=t, y=noise, spar=1)$y
	noise <- interp1(t[noise <= cutoff], noise[noise <= cutoff], xi=t,
		method="linear", extrap=median(noise, na.rm=TRUE))
	is.max <- localMaximaLogical(x, window=window)
	peaks <- is.max & (x / noise) >= SNR
	peaks[is.na(peaks)] <- FALSE
	list(peaks=peaks, noise=noise)
}

peakPick.limpic <- function(x, SNR=6, window=5, blocks=100, thresh=0.75, ...) {
	t <- seq_along(x)
	xint <- blocks(x, blocks=blocks)
	# identify flat reginos of spectrum
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
	is.max <- localMaximaLogical(x, window=window)
	peaks <- is.max & (x / noise) >= SNR
	peaks[is.na(peaks)] <- FALSE
	halfWindow <- floor(window / 2)
	# eliminate smooth peaks
	cutoff <- ceiling(halfWindow / 2) * quantile(abs(diff(x)), thresh) / 2
	too.flat <- sapply(which(peaks), function(i) {
		if ( (i - halfWindow) < 0 || (i + halfWindow) > length(x) ) {
			TRUE	
		} else {
			ratio <- x[i] - 0.5 * (x[i - halfWindow] + x[i + halfWindow])
			ratio < cutoff
		}
	})
	# return peak list
	peaks[which(peaks)[too.flat]] <- FALSE
	list(peaks=peaks, noise=noise)
}
