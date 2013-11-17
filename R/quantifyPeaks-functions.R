
#### function for quantifying uncertainty about peaks in a signal ####

quantifyPeaks <- function(x, t, peaks, bounds, span) {
	if ( missing(bounds) && missing(peaks) ) {
		stop("either 'peaks' or 'bounds' must be specified")
	} else if ( missing(bounds) ) {
		if ( !isTRUE(any(peaks)) ) return(emptyMSPeakFrame())
		bounds <- nearestLocalMaxima(-x, seq_along(x), which(peaks), span=span)
	} else if ( missing(peaks) ) {
		peaks <- rep(FALSE, length(x))
		peaks[localMaximaWithinBounds(x, bounds[,1], bounds[,2])] <- TRUE
	}
	duplicate <- duplicated(bounds[,1]) | duplicated(bounds[,2])
	bounds <- bounds[!duplicate,,drop=FALSE]
	peaks <- which(peaks)[!duplicate]
	area <- mapply(function(l, u) sum(x[l:u]), bounds[,1], bounds[,2])
	sigma <- mapply(function(l, u) sqrt(groupVar(t[l:u], x[l:u])), bounds[,1], bounds[,2])
	mu <- mapply(function(l, u) groupMean(t[l:u], x[l:u]), bounds[,1], bounds[,2])
	fwhm <- 2 * sqrt(2 * log(2)) * sigma
	peak.frame <- data.frame(mz=t[peaks], intensity=x[peaks], mu=mu, sigma=sigma,
		area=area, fwhm=fwhm, lbound=t[bounds[,1]], ubound=t[bounds[,2]])
	rownames(peak.frame) <- NULL
	metaData <- list(indices=list(peaks=peaks, lbound=bounds[,1], ubound=bounds[,2]))
	new("MSPeakFrame", peaks=peak.frame, metaData=metaData)
}
