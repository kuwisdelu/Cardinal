
#### functions for peak detection ####

detectPeaksFunction <- function(method) {
	if ( is.function(method) ) {
		return(method)
	} else if ( is.character(method) ) {
		if ( length(method) > 1 ) method <- method[[1]]
		switch(method,
			"snr" = detectPeaksSNR,
			"limpic" = detectPeaksLIMPIC,
			match.fun(method)
		)
	} else {
		stop("could not find matching function for ", substitute(method))
	}
}

detectPeaksSNR <- function(x, t, noise=c("sd", "mad", "adaptive-sd", "adaptive-mad",
	"limpic", "supersmoother"), snr=6, span=5, ...)
{
	if ( missing(t) ) t <- 1:length(x)
	fun <- estimateNoiseFunction(noise)
	# identify signal with strong snr
	noise <- fun(x, t, ...)
	is.peak <- (x / noise) > snr
	# return those which are also local maxima
	is.max <- localMaximaLogical(x, span=span)
	peaks <- is.max & is.peak
	peaks[is.na(peaks) | is.nan(peaks)] <- FALSE
	peaks
}

detectPeaksLIMPIC <- function(x, t, snr=6, span=5, ...) {
	if ( missing(t) ) t <- 1:length(x)
	# identify peak candidates
	noise <- estimateNoiseLIMPIC(x, t, ...)
	is.peak <- (x / noise) > snr
	is.max <- localMaximaLogical(x, span=span)
	peaks <- is.max & is.peak
	# remove smoothest peaks
	halfspan <- floor(span / 2)
	height.thresh <- ceiling(halfspan / 2) * quantile(abs(diff(x)), 0.75) / 2
	too.flat <- sapply(which(peaks), function(i) {
		if ( i - halfspan < 0 | i + halfspan > length(x) ) return(TRUE)
		ratio <- x[i] - 0.5 * (x[i - halfspan] + x[i + halfspan])
		return(ratio < height.thresh)
	} )
	peaks[which(peaks)[too.flat]] <- FALSE
	peaks
}

