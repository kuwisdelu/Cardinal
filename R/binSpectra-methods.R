
#### implement methods for binning mass spectra ####

setMethod("binSpectra", c("MSImageSet", "missing"), function(object, pixel, coord,
	width=1, offset=0, filter=c("none", "gaussian", "sgolay", "kaiser", "ma"),
	plot=FALSE, ...)
{
	if ( offset > width ) {
		stop ("'offset' must be less than 'width'")
	}
	filter <- filterFunction(filter)
	mz.min <- ceiling(min(mz(object)))
	mz.max <- floor(max(mz(object)))
	bounds <- seq(from=mz.min, to=mz.max, by=width) + offset
	lbound <- bounds[-length(bounds)]
	ubound <- bounds[-1]
	new.mz <- (lbound + ubound) / 2
	lbound <- features(object, mz=lbound)
	ubound <- features(object, mz=ubound)
	new.featureData <- data.frame(new.mz)
	names(new.featureData) <- object@metaData[["mzDimName"]]
	rownames(new.featureData) <- new.mz
	pixel <- getPixels(object, pixel, coord)
	coord <- Cardinal:::coord(object)[pixel,]
	new.metaData <- object@metaData
	new.metaData[["positionArray"]] <- generatePositionArray(coord)
	new.spectra <- new.env(parent=globalenv())
	mz <- force(mz(object))
	tryVerboseMessage("Binning mass spectra...")
	new.spectra$spectra <- internalSpectralApply(object, list(pixel=pixel, subscripts=2),
		function(s) {
			bin(filter(s, mz, ...), lbound, ubound)
		}
	)
	new.object <- new("MSImageSet", spectra=new.spectra, peaks=object@peaks,
		featureData=new.featureData, pixelData=object@pixelData[pixel,],
		metaData=new.metaData)
	if  ( plot ) {
		for ( i in seq_along(pixel) ) {
			plot(object, pixel=pixel[[i]], col="black", ...)
			plot(new.object, pixel=i, col="red", type='p', pch=20, cex=0.2, add=TRUE)
			legend("topright", legend=c("Original Spectrum", "Binned Spectrum"),
				fill=c("black", "red"))
		}
	}
	isBinned(new.object) <- TRUE
	new.object@metaData[["history"]][[date()]] <- generateHistory(match.call(call=sys.call(-1)))
	validObject(new.object)
	return(new.object)
} )

setMethod("binSpectra", c("MSImageSet", "MSPeakFrame"), function(object, peaks,
	pixel, coord, sum=TRUE, plot=FALSE, ...)
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
	tryVerboseMessage("Binning mass spectra to peaks...")
	new.spectra$spectra <- internalSpectralApply(object, list(pixel=pixel, subscripts=2),
		function(s) {
			s <- bin(s, lbound, ubound, sum=sum)
			if ( !sum ) length(s) * s / sum(s)
			s
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
			plot(object, pixel=pixel[[i]], col="black",
				ylim=range(new.object@spectra$spectra[,i], na.rm=TRUE), ...)
			plot(new.object, pixel=i, col="red", type='p', pch=20, add=TRUE)
			plot(new.object, pixel=i, col="red", type='h', lwd=0.5, add=TRUE)
			legend("topright", legend=c("Original Spectrum", "Binned Spectrum"),
				fill=c("black", "red"))
		}
	}
	isPeaks(new.object) <- TRUE
	isBinned(new.object) <- TRUE
	new.object@metaData[["history"]][[date()]] <- generateHistory(match.call(call=sys.call(-1)))
	validObject(new.object)
	return(new.object)
} )
