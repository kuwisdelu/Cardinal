
#### implement methods dealing with baseline removal ####

setMethod("removeBaseline", "MSImageSet", function(object, pixel, coord,
	method="interp", non.negative=TRUE, plot=FALSE, ...)
{
	fun <- estimateBaselineFunction(method)
	pixel <- getPixels(object, pixel, coord)
	coord <- Cardinal:::coord(object)[pixel,]
	mz <- force(mz(object))
	new.spectra <- new.env(parent=globalenv())
	tryVerboseMessage("Removing baseline...")
	new.spectra$spectra <- internalSpectralApply(object, list(pixel=pixel, subscripts=2),
		function(s) {
			s <- s - fun(s, mz, ...)
			if ( non.negative ) s <- pmax(s, 0)
			s
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
			lines(mz, object@spectra$spectra[,pixel[[i]]] - new.object@spectra$spectra[,i], col="red")
			plot(new.object, pixel=i, add=TRUE)
			legend("topright", legend=c("Original Spectrum", "Estimated Baseline",
				"Corrected Spectrum"), fill=c("gray", "red", "black"))
		}
	}
	new.object@metaData[["history"]][[date()]] <- generateHistory(match.call(call=sys.call(-1)))
	validObject(new.object)
	return(new.object)
} )

