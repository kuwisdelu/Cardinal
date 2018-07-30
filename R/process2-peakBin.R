
#### Bin spectra to reference peaks ####
## ------------------------------------

setMethod("peakBin", c("MSImagingExperiment", "numeric"),
	function(object, ref, type=c("height", "area"),
		tolerance = 200, units = c("ppm", "mz"), ...)
	{
		mz <- mz(object)
		tol <- switch(match.arg(units),
			ppm = c("relative" = tolerance * 1e-6),
			mz = c("absolute" = tolerance))
		type <- match.arg(type)
		peaks <- bsearch(ref, mz, tol=tol, tol.ref="key")
		peaks <- peaks[!is.na(peaks)]
		fun <- peakBin_fun(ref, tol, type, peaks, mz)
		postfun <- peakBin_postfun(ref, tol)
		plotfun <- peakBin_plotfun(ref, tol)
		object <- process(object, fun=fun, ...,
			label="peakBin", kind="pixel",
			postfun=postfun, plotfun=plotfun,
			delay=TRUE)
		object
	})

peakBin_plotfun <- function(ref, tol) {
	fun <- function(s2, s1, ...,
		main="Peak binning", xlab="m/z", ylab="")
	{
		mz <- mz(attr(s1, "mcols"))
		plot(range(mz), range(s2), main=main,
			xlab=xlab, ylab=ylab, type='n', ...)
		lines(mz, s1, col="gray", type='l')
		bins <- attr(s2, "bins")
		if ( !is.null(bins) ) {
			i <- unique(unlist(mapply(":", bins[[1]], bins[[2]])))
			lines(mz[i], s1[i], col=rgb(0, 0, 1, 0.25), type='h')
		}
		lines(ref, s2, col="red", type='h')
	}
	fun
}

peakBin_fun <- function(ref, tol, type, peaks, mz) {
	fun <- function(x, ...) {
		mzi <- mz(attr(x, "mcols"))
		if ( !identical(mz, mzi) ) {
			peaks <- bsearch(ref, mzi, tol=tol, tol.ref="key")
			peaks <- peaks[!is.na(peaks)]
		}
		f <- switch(type, height=max, area=sum)
		bounds <- nearestLocalMaxima(-x, seq_along(x), peaks)
		peaks <- bin(x, bins=bounds, fun=f)
		attr(peaks, "bins") <- bounds
		peaks
	}
	fun
}

peakBin_postfun <- function(ref, tol) {
	fun <- function(object, ans, ...) {
		if ( is.matter(ans) ) {
			data <- as(ans, "matter_matc")
		} else {
			data <- as.matrix(simplify2array(ans))
		}
		object <- MSImagingExperiment(data,
			featureData=MassDataFrame(mz=ref),
			pixelData=pixelData(object),
			metadata=metadata(object),
			processing=processingData(object),
			centroided=TRUE)
		if ( !is.null(spectrumRepresentation(object)) )
			spectrumRepresentation(object) <- "centroid spectrum"
		.message("binned to ", length(ref), " reference peaks")
		object
	}
	fun
}
