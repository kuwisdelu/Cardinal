
#### define a valid MSImageSet object ####

setValidity("MSImageSet", function(object) {
	errors <- NULL
	if ( nrow(spectra(object)) != nrow(featureData(object)) ) {
		errors <- c(errors, "# of rows in spectra and # of rows in featureData differ")
	}
	if ( ncol(spectra(object)) != nrow(pixelData(object)) ) {
		errors <- c(errors, "# of columns in spectra and # of rows in pixelData differ")
	}
	if ( is.null(metaData(object)[["mzDimName"]]) ) {
		errors <- c(errors, "metaData field 'mzDimName' is missing")
	} else if ( !is.character(metaData(object)[["mzDimName"]]) ) {
		errors <- c(errors, "metaData field 'mzDimName' is not of class 'character'")
	} else if ( any(!metaData(object)[["mzDimName"]] %in% names(featureData(object))) ) {
		errors <- c(errors, "one or more members of metaData field 'mzDimName' is not a column of featureData")
	}
	if ( is.null(metaData(object)[["coordDimNames"]]) ) {
		errors <- c(errors, "metaData field 'coordDimNames' is missing")
	} else if ( !is.character(metaData(object)[["coordDimNames"]]) ) {
		errors <- c(errors, "metaData field 'coordDimNames' is not of class 'character'")
	} else if ( any(!metaData(object)[["coordDimNames"]] %in% names(pixelData(object))) ){
		errors <- c(errors, "one or more members of metaData field 'coordDimNames' is not a column of pixelData")
	}
	temp.positions <- generatePositionArray(coord(object))
	if ( any(dim(temp.positions) != dim(metaData(object)[["positionArray"]])) ) {
		warning("metaData field 'positionArray' is out of sync; run regeneratePositions() to resync")
	} else if ( any(temp.positions != metaData(object)[["positionArray"]], na.rm=TRUE) ) {
		warning("metaData field 'positionArray' is out of sync; run regeneratePositions() to resync")
	}
	return(errors)
} )

