
#### implement methods dealing with noise removal and smoothing ####

setMethod("removeNoise", "MSImageSet", function(object, pixel, coord,
	method=c("gaussian", "sgolay", "kaiser", "ma"), plot=FALSE, ...)
{
	fun <- filterFunction(method)
	pixel <- getPixels(object, pixel, coord)
	coord <- Cardinal:::coord(object)[pixel,]
	mz <- force(mz(object))
	new.spectra <- new.env(parent=globalenv())
	tryVerboseMessage("Removing noise in mass spectra...")
	new.spectra$spectra <- internalSpectralApply(object, list(pixel=pixel, subscripts=2),
		function(s) {
			fun(s, mz, ...)
		}
	)
	new.metaData <- object@metaData
	new.metaData[["positionArray"]] <- generatePositionArray(coord)
	new.object <- new("MSImageSet", spectra=new.spectra, peaks=object@peaks,
		featureData=object@featureData, pixelData=object@pixelData[pixel,,drop=FALSE],
		metaData=new.metaData)
	if ( plot ) {
		for ( i in seq_along(pixel) ) {
			plot(object, pixel=pixel[[i]], col="gray", ...)
			plot(new.object, pixel=i, add=TRUE)
			legend("topright", legend=c("Original Spectrum", "Smoothed Spectrum"),
				fill=c("gray", "black"))
		}
	}
	new.object@metaData[["history"]][[date()]] <- generateHistory(match.call(call=sys.call(-1)))
	validObject(new.object)
	return(new.object)
} )
