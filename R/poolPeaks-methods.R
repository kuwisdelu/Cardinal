
#### implement methods for pooling peaks ####

setMethod("poolPeaks", "MSPeakFrame", function(peaks, ...) {
	peaks <- list(peaks, ...)
	peaks <- lapply(peaks, function(p) p@peaks)
	peaks <- do.call(rbind, peaks)
	peaks <- peaks[order(peaks$mz),]
	rownames(peaks) <- NULL
	peaks <- new("MSPeakFrame", peaks=peaks, metaData=generateMetaData("MSPeakFrame"))
	return(peaks)
} )

setMethod("poolPeaks", "MSPeakList", function(peaks, ...) {
	peaklist <- list(peaks, ...)
	peaklist <- lapply(peaklist, function(p) p@peaks)
	peaklist <- unlist(peaklist, recursive=FALSE)
	peaklist <- new("MSPeakList", peaks=peaklist, metaData=generateMetaData("MSPeakList"))
	return(peaklist)
} )
