
#### implement methods for resampling mass spectra ####

setMethod("resampleSpectra", c("MSImageSet", "missing"), function(object, pixel, coord,
	step=1, offset=0, filter=c("none", "gaussian", "sgolay", "kaiser", "ma"),
	plot=FALSE, ...)
{
	if ( offset > step ) {
		stop ("'offset' must be less than 'step'")
	}
	filter <- filterFunction(filter)
	new.mz <- seq(from=ceiling(min(mz(object))), to=floor(max(mz(object))), by=step)
	new.mz <- new.mz[-length(new.mz)] + offset
	new.featureData <- data.frame(new.mz)
	names(new.featureData) <- object@metaData[["mzDimName"]]
	rownames(new.featureData) <- new.mz
	pixel <- getPixels(object, pixel, coord)
	coord <- Cardinal:::coord(object)[pixel,]
	new.metaData <- object@metaData
	new.metaData[["positionArray"]] <- generatePositionArray(coord)
	new.spectra <- new.env(parent=globalenv())
	mz <- force(mz(object))
	tryVerboseMessage("Resampling mass spectra...")
	new.spectra$spectra <- internalSpectralApply(object, list(pixel=pixel, subscripts=2),
		function(s) {
			interp1(x=mz, y=filter(s, mz, ...), xi=new.mz, ...)
		}
	)
	new.object <- new("MSImageSet", spectra=new.spectra, peaks=object@peaks,
		featureData=new.featureData, pixelData=object@pixelData[pixel,],
		metaData=new.metaData)
	if ( plot ) {
		for ( i in seq_along(pixel) ) {
			plot(object, pixel=pixel[i], col="black", ...)
			plot(new.object, pixel=i, col="red", type='p', pch=20, cex=0.2, add=TRUE)
			legend("topright", legend=c("Original Spectrum", "Resampled Spectrum"),
				fill=c("black", "red"))
		}
	}
	isResampled(new.object) <- TRUE
	new.object@metaData[["history"]][[date()]] <- generateHistory(match.call(call=sys.call(-1)))
	validObject(new.object)
	return(new.object)
} )

setMethod("resampleSpectra", c("MSImageSet", "MSPeakFrame"), function(object, peaks,
	pixel, coord, plot=FALSE, ...)
{
	new.mz <- peaks$mz
	new.featureData <- data.frame(new.mz)
	names(new.featureData) <- object@metaData[["mzDimName"]]
	rownames(new.featureData) <- new.mz
	lbound <- features(object, mz=new.mz - (peaks$fwhm / 2))
	ubound <- features(object, mz=new.mz + (peaks$fwhm / 2))
	pixel <- getPixels(object, pixel, coord)
	coord <- Cardinal:::coord(object)[pixel,]
	new.metaData <- object@metaData
	new.metaData[["positionArray"]] <- generatePositionArray(coord)
	new.spectra <- new.env(parent=globalenv())
	tryVerboseMessage("Resampling mass spectra to peaks...")
	new.spectra$spectra <- internalSpectralApply(object, list(pixel=pixel, subscripts=2),
		function(s) {
			s[localMaximaWithinBounds(s, lbound, ubound)]
		}
	)
	if ( !is.matrix(new.spectra$spectra) ) {
		new.spectra$spectra <- as.matrix(new.spectra$spectra)
	}
	new.object <- new("MSImageSet", spectra=new.spectra, peaks=peaks,
		featureData=new.featureData, pixelData=object@pixelData[pixel,],
		metaData=new.metaData)
	if ( plot ) {
		for ( i in seq_along(pixel) ) {
			plot(object, pixel=pixel[i], col="black",
				ylim=range(new.object@spectra$spectra[,i], na.rm=TRUE), ...)
			plot(new.object, pixel=i, col="red", type='p', pch=20, add=TRUE)
			plot(new.object, pixel=i, col="red", type='h', lwd=0.5, add=TRUE)
			legend("topright", legend=c("Original Spectrum", "Resampled Spectrum"),
				fill=c("black", "red"))
		}
	}
	isPeaks(new.object) <- TRUE
	isResampled(new.object) <- TRUE
	new.object@metaData[["history"]][[date()]] <- generateHistory(match.call(call=sys.call(-1)))
	validObject(new.object)
	return(new.object)
} )

