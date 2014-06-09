
#### Peak picking methods ####
## ---------------------------

setMethod("peakPick", "MSImageSet",
	function(object, method = c("simple", "adaptive", "limpic"),
		...,
		pixel=pixels(object),
		plot=FALSE)
	{
		fun <- peakPick.method(method)
		iData <- list()
		mzData <- list()
		data <- pixelApply(object, function(s) {
			pout <- fun(s, ...)
			if ( plot ) {
				wrap(plot(mz(object), s, type="l", xlab="m/z", ylab="Intensity", col="gray", ...),
					..., signature=fun)
				wrap(lines(mz(object), pout$noise, col="blue", ...),
					..., signature=fun)
				wrap(lines(mz(object)[pout$peaks], s[pout$peaks], col="red", type='h', ...),
					..., signature=fun)
			}
			intensityArray <- s[pout$peaks]
			names(intensityArray) <- featureNames(object)[pout$peaks]
			mzArray <- mz(object)[pout$peaks]
			names(mzArray) <- featureNames(object)[pout$peaks]
			iData <<- append(iData, list(intensityArray))
			mzData <<- append(mzData, list(mzArray))
			NULL
		}, .pixel=pixel, ..., .use.names=FALSE)
		iData <- new("Hashmat", data=iData, keys=featureNames(object),
			dim=c(length(features(object)), length(iData)))
		mzData <- new("Hashmat", data=mzData, keys=featureNames(object),
			dim=c(length(features(object)), length(mzData)))
		object@imageData <- SImageData(data=iData,
			coord=coord(object)[pixel,],
			storageMode=storageMode(object@imageData),
			dimnames=list(featureNames(object), pixelNames(object)[pixel]))
		object@imageData[[".mzData"]] <- mzData
		object@pixelData <- object@pixelData[pixel,]
		peakPicking(processingData(object)) <- match.method(method)
		prochistory(processingData(object)) <- .history()
		object
	})

peakPick.method <- function(method) {
	if ( is.character(method) ) {
		method <- switch(method[[1]],
			simple = peakPick.simple,
			adaptive = peakPick.adaptive,
			limpic = peakPick.limpic,
			match.fun(method))
	}
	match.fun(method)
}

peakPick.simple <- function(x, snr=6, window=5, blocks=100, ...) {
	t <- seq_along(x)
	xint <- intervals(x, blocks=blocks)
	kurt <- sapply(xint, kurtosis) - 3
	noise <- mean(sapply(xint, sd)[kurt < 1], na.rm=TRUE)
	noise <- rep(noise, length(x))
	is.max <- localMaximaLogical(x, span=window)
	peaks <- is.max & (x / noise) >= snr
	peaks[is.na(peaks)] <- FALSE
	list(peaks=peaks, noise=noise)
}

peakPick.adaptive <- function(x, snr=6, window=5, blocks=100, spar=1, ...) {
	t <- seq_along(x)
	xint <- intervals(x, blocks=blocks)
	xlen <- sapply(xint, length)
	noise <- sapply(xint, sd)
	noise <- unlist(mapply(function(ns, ln) rep(ns, ln), noise, xlen))
	cutoff <- smooth.spline(x=t, y=noise, spar=spar)$y
	noise <- interp1(t[noise <= cutoff], noise[noise <= cutoff], xi=t,
		method="linear", extrap=median(noise, na.rm=TRUE))
	is.max <- localMaximaLogical(x, span=window)
	peaks <- is.max & (x / noise) >= snr
	peaks[is.na(peaks)] <- FALSE
	list(peaks=peaks, noise=noise)
}

peakPick.limpic <- function(x, snr=6, window=5, blocks=100, ...) {
	t <- seq_along(x)
	xint <- intervals(x, blocks=blocks)
	kurt <- sapply(xint, kurtosis) - 3
	means <- sapply(xint, mean)
	is.flat <- kurt < 1 & means < mean(x)
	noiseval <- sapply(xint, sd)[is.flat]
	noiseval <- c(median(noiseval, na.rm=TRUE), noiseval, median(noiseval, na.rm=TRUE))
	noiseidx <- floor(seq(from=blocks/2, to=length(x) - blocks/2,
		length.out=length(xint)))[is.flat]
	noiseidx <- c(1, noiseidx, length(x))
	noise <- interp1(x=t[noiseidx], y=noiseval, xi=t, method="linear")
	is.max <- localMaximaLogical(x, span=window)
	peaks <- is.max & (x / noise) >= snr
	peaks[is.na(peaks)] <- FALSE
	halfWindow <- floor(window / 2)
	cutoff <- ceiling(halfWindow / 2) * quantile(abs(diff(x)), 0.75) / 2
	too.flat <- sapply(which(peaks), function(i) {
		if ( (i - halfWindow) < 0 || (i + halfWindow) > length(x) ) {
			TRUE	
		} else {
			ratio <- x[i] - 0.5 * (x[i - halfWindow] + x[i + halfWindow])
			ratio < cutoff
		}
	})
	peaks[which(peaks)[too.flat]] <- FALSE
	list(peaks=peaks, noise=noise)
}
