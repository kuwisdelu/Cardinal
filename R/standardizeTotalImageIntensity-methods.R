
#### implement methods for standardizing the TII ####

setMethod("standardizeTotalImageIntensity",  "MSImageSet", function(object, value=numPixels(object),
	sliceDimNames=metaData(object)[["coordDimNames"]][c(1,2)])
{
	fixed <- object@metaData[["coordDimNames"]][!object@metaData[["coordDimNames"]] %in% sliceDimNames]
	coord <- rep(list(NA), length(object@metaData[["coordDimNames"]]))
	names(coord) <- object@metaData[["coordDimNames"]]
	coord[[sliceDimNames[[1]]]] <- 1:max(coord(object)[[sliceDimNames[[1]]]])
	coord[[sliceDimNames[[2]]]] <- 1:max(coord(object)[[sliceDimNames[[2]]]])
	fixed.min <- sapply(coord(object)[fixed], min)
	fixed.max <- sapply(coord(object)[fixed], max)
	fixcoord <- lapply(seq_along(fixed), function(i) fixed.min[[i]]:fixed.max[[i]])
	fixcoord <- do.call(expand.grid, fixcoord)
	names(fixcoord) <- fixed
	new.spectra <- new.env(parent=globalenv())
	new.spectra$spectra <- matrix(nrow=numFeatures(object), ncol=numPixels(object))
	tryVerboseProgress(start=TRUE, total=numFeatures(object))
	for ( i in 1:numFeatures(object) ) {
		tryVerboseProgress(increment=TRUE)
		for ( j in 1:nrow(fixcoord) ) {
			coordij <- coord
			coordij[fixed] <- fixcoord[j,fixed]
			pixel <- pixels(object, coord=coordij)
			pixel <- pixel[is.finite(pixel)]
			s <- spectra(object)[i,pixel]
			if ( max(s) > 0 ) {
				new.spectra$spectra[i,pixel] <- value * s / sum(s)
			} else {
				new.spectra$spectra[i,pixel] <- rep(0, length(pixel))
			}
		}
	}
	tryVerboseProgress(stop=TRUE)
	new.object <- new("MSImageSet", spectra=new.spectra, peaks=object@peaks,
		featureData=object@featureData, pixelData=object@pixelData,
		metaData=object@metaData)
	new.object@metaData[["history"]][[date()]] <- Cardinal:::generateHistory(match.call(call=sys.call(-1)))
	validObject(new.object)
	return(new.object)
} )
