
#### implement methods for baseline estimation ####

setMethod("estimateBaseline", "MSImageSet", function(object, pixel, coord,
	method="interp", plot=FALSE, ...)
{
	fun <- estimateBaselineFunction(method)
	pixel <- getPixels(object, pixel, coord)
	mz <- force(mz(object))
	tryVerboseMessage("Estimating baseline...")
	baseline <- internalSpectralApply(object, list(pixel=pixel, subscripts=2),
		function(s) {
			fun(s, mz, ...)
		}
	)
	if ( plot ) {
		for ( i in seq_along(pixel) ) {
			plot(object, pixel=pixel[[i]], col="black", ...)
			lines(mz, baseline[,i], lwd=2, col="red")
			legend("topright", legend=c("Mass Spectrum", "Estimated Baseline"),
				fill=c("black", "red"))
		}
	}
	return(baseline)
} )
