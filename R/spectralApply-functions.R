
#### functions for apply-like functionality ####

internalSpectralApply <- function(object, MARGIN, FUN, ...) {
	if ( is.null(MARGIN$pixel) ) MARGIN$pixel <- 1:numPixels(object)
	if ( is.null(MARGIN$feature) ) MARGIN$feature <- 1:numFeatures(object)
	jobsize <- if ( MARGIN$subscripts == 1 ) length(MARGIN$feature) else length(MARGIN$pixel)
	tryVerboseProgress(start=TRUE, total=jobsize)
	NEWFUN <- function(s, ...) {
		tryVerboseProgress(increment=TRUE)
		FUN(s, ...)
	}
	if ( length(MARGIN$pixel) == numPixels(object) && length(MARGIN$feature) == numFeatures(object) ) {
		out <- apply(object@spectra$spectra, MARGIN$subscripts, NEWFUN, ...)
	} else {
		out <- apply(object@spectra$spectra[MARGIN$feature,MARGIN$pixel,drop=FALSE],
			MARGIN$subscripts, NEWFUN, ...)
	}
	tryVerboseProgress(stop=TRUE)
	out
}
