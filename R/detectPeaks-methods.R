
#### implement methods for peak detection ####

setMethod("detectPeaks", "MSImageSet", function(object, pixel, coord,
	method=c("snr", "limpic"), plot=FALSE, ...)
{
	fun <- detectPeaksFunction(method)
	pixel <- getPixels(object, pixel, coord)
	mz <- force(mz(object))
	tryVerboseMessage("Detecting peaks in mass spectra...")
	peaks <- internalSpectralApply(object, list(pixel=pixel, subscripts=2),
		function(s) {
			p <- fun(s, mz, ...)
			quantifyPeaks(s, mz, p, span=5)
		}
	)
	peaks <- new("MSPeakList", peaks=peaks, metaData=generateMetaData("MSPeakList"))
	peaks@metaData$method <- method
	peaks@metaData$call <- generateHistory(match.call(call=sys.call(-1)))
	if ( plot ) {
		for ( i in seq_along(pixel) ) {
			plot(object, pixel=pixel[i], col="black", ...)
			plot(peaks[[i]], dist=FALSE, col="red", add=TRUE)
			legend("topright", legend=c("Mass Spectrum", "Peaks"),
				fill=c("black", "red"))
		}
	}
	return(peaks)
} )

