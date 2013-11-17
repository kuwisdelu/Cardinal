
#### implement intensities methods ####

# return an array of intensities for the mass spectra at given parameters
setMethod("intensities", c("MSImageSet"), function(object, pixel, coord, mz, feature,
	drop=FALSE)
{
	pixel <- getPixels(object, pixel, coord, drop=drop)
	feature <- getFeatures(object, feature, mz)
	dimensions <- dim(pixel)
	spectra.array <- object@spectra$spectra[feature,pixel[is.finite(pixel)],drop=FALSE]
	if ( drop ) {
		if ( !is.null(dimensions) && any(dimensions == 1) ) {
			spectra.array <- as.numeric(spectra.array)
		} else {
			dim(spectra.array) <- c(length(feature), sum(is.finite(pixel)))
		}
	} else {
		pixel[is.finite(pixel)] <- seq_len(sum(is.finite(pixel)))
		spectra.array <- spectra.array[,pixel,drop=FALSE]
		dim(spectra.array) <- c(length(feature), dimensions)
	}
	return(spectra.array)
} )

#### helper functions ####

getPixels <- function(object, pixel, coord, missingok=TRUE, drop=TRUE) {
	if ( missing(pixel) && missing(coord) ) {
		pixel <- metaData(object)[["positionArray"]]
		if ( !missingok ) pixel <- pixel[is.finite(pixel)]
	} else if ( missing(pixel) ) {
		pixel <- pixels(object, coord, drop=FALSE)
	} else if ( is.logical(pixel) ) {
		pixel <- which(pixel)
	}
	if ( drop ) {
		sort(unique(pixel))
	} else {
		if ( length(dim(pixel)) != length(metaData(object)[["coordDimNames"]]) ) {
			pixel <- getEnclosingCube(object, pixel)
			if ( any(is.na(pixel)) && !missingok ) {
				stop("cannot avoid missing pixels with 'drop' set to TRUE")
			}
		}
		pixel
	}
}

getFeatures <- function(object, feature, mz, missingok=TRUE) {
	if ( missing(feature) && missing(mz) && missingok ) {
		feature <- 1:numFeatures(object)
	} else if ( missing(feature) ) {
		feature <- features(object, mz[[1]]):features(object, mz[[length(mz)]])
	} else if ( is.logical(feature) ) {
		feature <- which(feature)
	}
	sort(unique(feature))
}

getEnclosingCube <- function(object, pixel) {
	xyz <- coord(object)[pixel,]
	cube <- lapply(xyz, function(xyzs) min(xyzs):max(xyzs))
	f <- function(...) metaData(object)[["positionArray"]][...,drop=FALSE]
	do.call(f, cube)
}
