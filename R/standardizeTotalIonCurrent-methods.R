
#### implement methods for standardizing the TIC ####

setMethod("standardizeTotalIonCurrent", "MSImageSet", function(object, value=numFeatures(object)) {
	new.spectra <- new.env(parent=globalenv())
	tic <- apply(object@spectra$spectra, 2, sum)
	new.spectra$spectra <- internalSpectralApply(object, list(subscripts=2),
		function(s) {
			tic <- sum(s)
			if ( tic > 0 ) {
				value * s / tic
			} else {
				rep(0, length(s))
			}
		}
	)
	new.object <- new("MSImageSet", spectra=new.spectra, peaks=object@peaks,
		featureData=object@featureData, pixelData=object@pixelData,
		metaData=object@metaData)
	new.object@metaData[["history"]][[date()]] <- generateHistory(match.call(call=sys.call(-1)))
	new.object@pixelData$TIC <- tic
	validObject(new.object)
	return(new.object)
} )
