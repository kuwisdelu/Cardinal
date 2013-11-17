
#### implement methods for merging peaks ####

setMethod("mergePeaks", "MSPeakList", function(peaks, length.min=1, ...) {
	if ( length.min < 1 ) stop ("'length.min' must be >= 1")
	length <- sapply(peaks@peaks, function(x) nrow(x@peaks))
	peaks <- peaks[length > length.min]
	peaklist <- lapply(peaks@peaks, function(x) mergePeaks(x))
	peaks <- do.call(poolPeaks, peaklist)
	rownames(peaks) <- NULL
	return(peaks)
} )

setMethod("mergePeaks", "MSPeakFrame", function(peaks, ...) {
	intensity <- max(peaks$intensity)
	mz <- combinedMean(peaks$mz, peaks$intensity)
	mu <- combinedMean(peaks$mu, peaks$area)
	sigma <- sqrt(combinedVar(peaks$mu, peaks$sigma^2, peaks$area))
	area <- sum(peaks$area)
	fwhm <- 2 * sqrt(2 * log(2)) * sigma
	lbound <- combinedMean(peaks$lbound, peaks$area)
	ubound <- combinedMean(peaks$ubound, peaks$area)
	peak.frame <- data.frame(mz=mz, intensity=intensity, mu=mu, sigma=sigma, area=area,
		fwhm=fwhm, lbound=lbound, ubound=ubound)
	peaks <- new("MSPeakFrame", peaks=peak.frame, metaData=peaks@metaData)
	return(peaks)
} )
