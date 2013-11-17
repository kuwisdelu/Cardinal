
setMethod("sliceBind", "MSImageSet", function(object, ..., newDim=TRUE,
	newDimName=paste("x", length(metaData[["coordDimNames"]]) + 1,
	sep=""))
{
	oblist <- list(object, ...)
	if ( length(oblist) < 2 ) return(object)
	for ( i in seq_along(oblist) ) {
		if ( ! isTRUE(all.equal(mz(object), mz(oblist[[i]]))) ) {
			stop("m/z values for all objects must match")
		}
		if ( any(sort(names(object@pixelData)) != sort(names(oblist[[i]]@pixelData))) ) {
			stop("names of columns in 'pixelData' must match")
		}
	}
	featureData <- lapply(oblist, function(x) {
		features <- x@featureData
		features[x@metaData[["mzDimName"]]] <- NULL
		features
	} )
	featureData <- do.call(data.frame, featureData)
	featureData[object@metaData[["mzDimName"]]] <- mz(object)
	pixelData <- lapply(oblist, function(x) x@pixelData[names(object@pixelData)])
	pixelData <- do.call(rbind, pixelData)
	metaData <- object@metaData
	if ( newDim ) {
		newCoord <- lapply(seq_along(oblist), function(i) rep(i, numPixels(oblist[[i]])))
		newCoord <- unlist(newCoord)
		pixelData[newDimName] <- newCoord
		metaData[["coordDimNames"]] <- c(metaData[["coordDimNames"]], newDimName)
	}
	metaData[["name"]] <- character()
	metaData[["positionArray"]] <- generatePositionArray(pixelData[
		metaData[["coordDimNames"]]])
	metaData[["history"]][[date()]] <- generateHistory(match.call(call=sys.call(-1)))
	spectra <- new.env(parent=globalenv())
	slist <- lapply(oblist, function(x) x@spectra$spectra)
	spectra$spectra <- do.call(cbind, slist)
	new("MSImageSet", spectra=spectra, peaks=object@peaks, featureData=featureData,
		pixelData=pixelData, metaData=metaData)
} )
