
#### implement methods dealing with noise removal and smoothing ####

setMethod("estimateNoise", "MSImageSet", function(object, pixel, coord,
	method=c("sd", "mad", "adaptive-sd", "adaptive-mad", "limpic", "supersmoother"),
	snr=6, plot=FALSE, ...)
{
	fun <- estimateNoiseFunction(method)
	pixel <- getPixels(object, pixel, coord)
	mz <- force(mz(object))
	tryVerboseMessage("Estimating noise level in mass spectra...")
	noise <- internalSpectralApply(object, list(pixel=pixel, subscripts=2),
		function(s) {
			fun(s, mz, ...)
		}
	)
	if ( plot ) {
		for ( i in seq_along(pixel) ) {
			plot(object, pixel=pixel[[i]], col="black", ...)
			signal <- spectra(object)[,pixel[[i]]]
			which <- signal / noise[,i] > snr
			points(mz[which], signal[which], type="h", col="green")
			lines(mz, noise[,i], type="l", col="blue")
			legend("topright", legend=c("Mass Spectrum", "Estimated Noise",
				paste("SNR >", snr)), fill=c("black", "blue", "green"))
		}
	}
	return(noise)
} )
