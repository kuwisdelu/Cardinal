
#### implement methods dealing with assessing spectral quality ####

setMethod("assessQuality", "MSImageSet", function(object, pixel, coord,
	method="snr", plot=FALSE, ...)
{
	fun <- assessQualityFunction(method)
	pixel <- getPixels(object, pixel, coord)
	mz <- force(mz(object))
	tryVerboseMessage("Assessing mass spectral quality...")
	scores <- internalSpectralApply(object, list(pixel=pixel, subscripts=2),
		function(s) {
			fun(s, mz, ...)
		}
	)
	if ( plot ) {
		for ( i in seq_along(pixel) ) {
			plot(object, pixel=pixel[[i]], ...)
			legend("topright", legend=paste("Quality Score = ", round(scores[[i]],
				digits=4)))
		}
	}
	return(scores)
} )
