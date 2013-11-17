
#### Implements methods for coregistration of 3D sections ####

setMethod("coregisterImages", "MSImageSet", function(object, sliceDimNames=metaData(object)[[
	"coordDimNames"]][c(1,2)], refCoord, registerCoord=list(), ...)
{
	if ( (length(sliceDimNames)+length(registerCoord)) < length(object@metaData[["coordDimNames"]]) ) {
		regMoreCoord <- lapply(coord(object)[!(object@metaData[["coordDimNames"]] %in% sliceDimNames)], function(xyz) 1:max(xyz))
		names(regMoreCoord) <- object@metaData[["coordDimNames"]][!(object@metaData[["coordDimNames"]] %in% sliceDimNames)]
		regMoreCoord[names(regMoreCoord) %in% names(registerCoord)] <- registerCoord
		registerCoord <- regMoreCoord
	}
	sliceCoord <- do.call(expand.grid, registerCoord)
	if ( missing(refCoord) ) refCoord <- sliceCoord[1,]
	slicePixels <- apply(sliceCoord, 1, function(xyz) {
		xyz <- as.list(xyz)
		names(xyz) <- names(registerCoord)
		pix <- pixels(object, coord=xyz)
		pix[is.finite(pix)]
	} )
	if ( is.matrix(slicePixels) ) slicePixels <- lapply(1:ncol(slicePixels), function(i) slicePixels[,i])
	whichRef <- which(apply(sliceCoord, 1, function(xyz) all(as.numeric(refCoord) == xyz)))
	if ( length(whichRef) == 0 ) stop("'refCoord' must appear in 'registerCoord'")
	for ( i in (whichRef - 1):1 ) {
		if ( i < 1 ) break
		tryVerboseMessage("Coregistering ", paste(names(registerCoord), "=",
			unlist(sliceCoord[i,]), collapse=", "), " with ",
			paste(names(registerCoord), "=", unlist(sliceCoord[i + 1,]),
			collapse=", "), precedes.progress.output=FALSE)
		par <- findTransformation(object, sliceDimNames, slicePixels[[i + 1]], slicePixels[[i]])
		object <- rotateSlice(object, slicePixels[[i]], sliceDimNames, par[1:2], par[3])
	}
	for ( i in (whichRef + 1):length(slicePixels) ) {
		if ( i > length(slicePixels) ) break
		tryVerboseMessage("Coregistering ", paste(names(registerCoord), "=",
			unlist(sliceCoord[i,]), collapse=", "), " with ",
			paste(names(registerCoord), "=", unlist(sliceCoord[i - 1,]),
			collapse=", "), precedes.progress.output=FALSE)
		par <- findTransformation(object, sliceDimNames, slicePixels[[i - 1]], slicePixels[[i]])
		object <- rotateSlice(object, slicePixels[[i]], sliceDimNames, par[1:2], par[3])
	}
	positionArray <- generatePositionArray(object@pixelData[
		object@metaData[["coordDimNames"]]])
	kept <- seq_len(numPixels(object)) %in% positionArray
	new.spectra <- new.env(parent=globalenv())
	new.spectra$spectra <- object@spectra$spectra[,kept,drop=FALSE]
	object@spectra <- new.spectra
	object@pixelData <- object@pixelData[kept,,drop=FALSE]
	if ( sum(!kept) > 0 ) warning(sum(!kept), " pixel(s) dropped")
	object@metaData[["history"]][[date()]] <- generateHistory(match.call(call=sys.call(-1)))
	object@metaData[["positionArray"]] <- generatePositionArray(object@pixelData[
		object@metaData[["coordDimNames"]]])
	validObject(object)
	return(object)
} )
