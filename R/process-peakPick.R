
#### Peak picking methods ####
## ---------------------------

setMethod("peakPick", "MSImagingExperiment",
	function(object, method = c("simple", "adaptive"), ...)
	{
		fun <- peakPick.method2(method)
		e <- new.env(parent=getNamespace("Cardinal"))
		e$mz <- mz(object)
		plotfun <- function(s1, s2, ...,
			main="Peak picking", xlab="m/z", ylab="")
		{
			plot(mz, s2, main=main, xlab=xlab, ylab=ylab,
				col="gray", type='l', ...)
			lines(mz[s1], s2[s1], col="red", type='h')
		}
		environment(plotfun) <- e
		postfun <- function(peaks, object, ...) {
			mzData <- lapply(peaks, function(idx) mz(object)[idx])
			intensityData <- lapply(seq_along(peaks), function(i) {
				idx <- peaks[[i]]
				s <- spectra(object)[idx,i]
				as.numeric(s)
			})
			data <- list(keys=mzData, values=intensityData)
			data <- sparse_mat(data, keys=mz(object),
				nrow=nrow(object), ncol=ncol(object))
			tolerance(data) <- .Machine$double.eps
			object <- as(object, "MSImagingExperiment")
			imageData(object) <- MSProcessedImagingSpectraList(data)
			as(object, "MSProcessedImagingExperiment")
		}
		environment(postfun) <- getNamespace("Cardinal")
		object <- process(object, fun=fun, ...,
			label="peakPick", kind="pixel",
			postfun=postfun, plotfun=plotfun,
			delay=TRUE)
		object
	})

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

peakPick.simple <- function(x, SNR=6, window=5, blocks=100, ...) {
	noise <- .estimateNoiseSimple(x, blocks=blocks)
	is.max <- localMaximaLogical(x, window=window)
	peaks <- is.max & (x / noise) >= SNR
	peaks[is.na(peaks)] <- FALSE
	list(peaks=peaks, noise=noise)
}

peakPick.simple2 <- function(x, SNR=6, window=5, blocks=100, ...) {
	noise <- .estimateNoiseSimple(x, blocks=blocks)
	is.max <- localMaximaLogical(x, window=window)
	peaks <- is.max & (x / noise) >= SNR
	peaks[is.na(peaks)] <- FALSE
	which(peaks)
}

peakPick.adaptive <- function(x, SNR=6, window=5, blocks=100, spar=1, ...) {
	noise <- .estimateNoiseAdaptive(x, blocks=blocks, spar=spar)
	is.max <- localMaximaLogical(x, window=window)
	peaks <- is.max & (x / noise) >= SNR
	peaks[is.na(peaks)] <- FALSE
	list(peaks=peaks, noise=noise)
}

peakPick.adaptive2 <- function(x, SNR=6, window=5, blocks=100, spar=1, ...) {
	noise <- .estimateNoiseAdaptive(x, blocks=blocks, spar=spar)
	is.max <- localMaximaLogical(x, window=window)
	peaks <- is.max & (x / noise) >= SNR
	peaks[is.na(peaks)] <- FALSE
	which(peaks)
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
