
#### implement methods for peak alignment ####

setMethod("alignPeaks", c("MSPeakList", "MSImageSet"), function(peaks, reference,
	method=c("diff", "ztest", "dp"), fun=mean, plot=FALSE, ...)
{
	tryVerboseMessage("Generating mass spectral reference...")
	reference <- spectralApply(reference, 1, fun)
	tryVerboseMessage("Creating reference peaks...", precedes.progress.output=FALSE)
	lbound <- features(reference, unlist(lapply(peaks@peaks, function(p) p$lbound)))
	ubound <- features(reference, unlist(lapply(peaks@peaks, function(p) p$ubound)))
	spectrum <- spectra(reference)[,1]
	locmax <- localMaximaLogical(spectrum, span=median(ubound - lbound))
	refpeaks <- quantifyPeaks(spectrum, mz(reference), peaks=locmax, span=3)
	peaklist <- alignPeaks(peaks, refpeaks, method=method, plot=plot, ...)
	return(peaklist)
} )

setMethod("alignPeaks", c("MSPeakList", "MSPeakFrame"), function(peaks, reference,
	method=c("diff", "ztest", "dp"), plot=FALSE, ...)
{
	fun <- alignPeaksFunction(method)
	peaks <- peaks[sapply(peaks@peaks, function(p) nrow(p@peaks)) > 0]
	tryVerboseMessage("Aligning peaks to reference...")
	tryVerboseProgress(start=TRUE, total=length(peaks@peaks))
	align <- function(peaks, reference, ...) {
		tryVerboseProgress(increment=TRUE)
		fun(peaks, reference, ...)
	}
	matched <- lapply(peaks@peaks, align, reference=reference, ...)
	tryVerboseProgress(stop=TRUE)
	alignment <- rep(list(emptyMSPeakFrame()), nrow(reference@peaks))
	tryVerboseMessage("Collating aligned peak list...")
	tryVerboseProgress(start=TRUE, total=length(matched))
	for ( i in seq_along(matched) ) {
		tryVerboseProgress(increment=TRUE)
		alignment[matched[[i]]$reference] <- mapply(function(p, r) {
			poolPeaks(peaks[[i]][p,], alignment[[r]])
		}, matched[[i]]$peaks, matched[[i]]$reference)
	}
	tryVerboseProgress(stop=TRUE)
	peaklist <- new("MSPeakAlignment", peaks=alignment,
		metaData=generateMetaData("MSPeakList"))
	peaklist@metaData$reference <- reference
	if ( plot ) {
		plot(reference, type="n", nlabels=0, xlab="m/z", ylab="intensity", ...)
		for ( i in seq_along(peaks@peaks) ) {
			plot(peaks[[i]], nlabels=0, col="gray", add=TRUE)
		}
		plot(peaklist, add=TRUE)
		abline(v=reference$mz, lwd=0.2)
		plot(reference, dist=TRUE, add=TRUE)
		legend("topright", legend=c("Peaks", "Reference"),
			fill=c("gray", "black"))
	}
	return(peaklist)
} )

