
#### methods for selecting peaks ####

setMethod("selectPeaks", "MSImageSet", function(object, pixel, coord,
	silent=FALSE, ...)
{
	if ( missing(pixel) && missing(coord) ) {
		stop("either 'pixel' or 'coord' must be specified")
	} else if ( missing(pixel) ) {
		pixel <- pixels(x, coord)
	}
	if ( any(is.na(pixel)) ) stop("invalid pixel")
	if ( !silent ) message("Select peak boundaries and press ESC or second mouse button when done")
	lbound <- NULL
	ubound <- NULL
	min.mz <- min(mz(object))
	max.mz <- max(mz(object))
	loc <- NULL
	repeat {
		bounds <- numeric(2)
		for ( i in 1:2 ) {
			repeat {
				loc <- locator(1)
				if ( is.null(loc) || ( loc$x > min.mz && loc$x < max.mz ) ) break
			}
			if ( is.null(loc) ) break
			abline(v=loc$x, lty=4, lwd=0.5, col="blue")
			bounds[[i]] <- loc$x
		}
		if ( is.null(loc) ) break
		bounds <- sort(bounds)
		rect(bounds[[1]], par("usr")[[3]], bounds[[2]], par("usr")[[4]],
			col=rgb(0, 0, 1, 0.5))
		lbound <- c(lbound, bounds[[1]])
		ubound <- c(ubound, bounds[[2]])
	}
	if ( is.null(lbound) || is.null(ubound) ) stop("no peaks selected")
	peaks <- selectPeaksHelper(object, pixel, lbound, ubound)
	return(peaks)
} )

#### helper functions ####

selectPeaksHelper <- function(object, pixel, lbound, ubound) {
	lbound <- features(object, lbound)
	ubound <- features(object, ubound)
	quantifyPeaks(spectra(object)[,pixel], mz(object),
		bounds=cbind(lbound, ubound))
}
