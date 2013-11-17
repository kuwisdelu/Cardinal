
#### implement methods for apply-like functionality ####

setMethod("spectralApply", "MSImageSet", function(object, MARGIN, FUN, ...)
{
	new.spectra <- internalSpectralApply(object, MARGIN=list(subscripts=MARGIN), FUN=FUN, ...)
	if ( is.list(new.spectra) ) {
		new.object <- list(spectra=new.spectra, featureData=object@featureData,
			pixelData=object@pixelData, metaData=object@metaData)
	} else {
		if ( MARGIN == 1 ) {
			if ( is.matrix(new.spectra) ) {
				new.spectra <- t(new.spectra)
			} else {
				new.spectra <- as.matrix(new.spectra)
			}
			if ( all(dim(new.spectra) == dim(spectra(object))) ) {
				new.object <- createMSImageSet(new.spectra, mz=mz(object), coord=coord(object))
			} else {
				new.coord <- expand.grid(1:ncol(new.spectra), 1)
				names(new.coord) <- paste("x", 1:ncol(new.coord), sep="")
				new.object <- createMSImageSet(new.spectra, mz=mz(object), coord=new.coord)
			}
			new.object@featureData <- object@featureData
		} else if ( MARGIN == 2 ) {
			if ( !is.matrix(new.spectra) ) new.spectra <- t(new.spectra)
			new.mz <- 1:nrow(new.spectra)
			new.object <- createMSImageSet(new.spectra, mz=new.mz, coord=coord(object))
			new.object@pixelData <- object@pixelData
		} else {
			stop("invalid 'MARGIN' argument")
		}
		new.object@metaData[["name"]] <- object@metaData[["name"]]
		new.object@metaData[["history"]][[date()]] <- generateHistory(match.call(call=sys.call(-1)))
	}
	return(new.object)
} )

