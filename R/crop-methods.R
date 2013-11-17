
#### implement subset methods ####

setMethod("crop", "MSImageSet", function(object, pixel, coord, mz, feature, ...) {
	pixel <- getPixels(object, pixel, coord)
	coord <- Cardinal:::coord(object)[pixel,]
	feature <- getFeatures(object, feature, mz)
	mz <- Cardinal:::mz(object)[feature]
	new.spectra <- new.env(parent=globalenv())
	new.spectra$spectra <- object@spectra$spectra[feature,pixel,drop=FALSE]
	new.featureData <- object@featureData[feature,,drop=FALSE]
	new.pixelData = object@pixelData[pixel,,drop=FALSE]
	new.metaData <- object@metaData
	new.metaData[["positionArray"]] <- generatePositionArray(coord(object)[pixel,,drop=FALSE])
	new.metaData[["history"]][[date()]] <- generateHistory(match.call(call=sys.call(-1)))
	new.object <- new("MSImageSet", spectra=new.spectra, peaks=object@peaks,
		featureData=new.featureData, pixelData=new.pixelData,
		metaData=new.metaData)
	validObject(new.object)
	return(new.object)
} )

