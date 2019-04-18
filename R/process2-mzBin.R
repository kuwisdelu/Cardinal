
#### Bin spectra to reference m/z ####
## -----------------------------------

setMethod("mzBin", c("MSImagingExperiment", "numeric"),
	function(object, ref, width = 400, units = c("ppm", "mz"), fun=sum, ...)
	{
		units <- match.arg(units)
		FUN <- match.fun(fun)
		fun <- mzBin_fun(ref, width, units, FUN)
		if ( length(ref) >= nrow(object) ) {
			.warning("new dimension [", length(ref), "] is greater ",
				"than current dimension [", nrow(object), "]")
		}
		metadata(featureData(object))[["reference mz"]] <- ref
		object <- process(object, fun=fun,
			label="mzBin", kind="pixel",
			postfun=mzBin_postfun,
			plotfun=mzBin_plotfun,
			delay=getOption("Cardinal.delay"))
		object
	})

setMethod("mzBin", c("MSImagingExperiment", "missing"),
	function(object, from=min(mz(object)), to=max(mz(object)), by,
		resolution = 200, units = c("ppm", "mz"), fun=sum, ...)
	{
		units <- match.arg(units)
		if ( missing(by) )
			by <- switch(units, ppm=resolution * 2, mz=resolution)
		halfwidth <- by / 2
		ref <- switch(units,
			ppm = seq.ppm(from=from, to=to, ppm=halfwidth),
			mz = seq(from=from, to=to, by=2 * halfwidth))
		if ( length(ref) >= nrow(object) ) {
			.warning("new dimension [", length(ref), "] is greater ",
				"than current dimension [", nrow(object), "]")
		}
		mzBin(object, ref=ref, width=2 * halfwidth,
			units=units, fun=fun, ...)
	})

mzBin_fun <- function(ref, width, units, FUN) {
	fun <- function(x, ...) {
		mz <- mz(attr(x, "mcols"))
		halfwidth <- width / 2
		if ( units == "ppm" ) {
			upper <- ref + 1e-6 * halfwidth * ref
			lower <- ref - 1e-6 * halfwidth * ref
		} else {
			upper <- ref + halfwidth
			lower <- ref - halfwidth
		}
		bin_vector(x, mz, list(lower, upper), fun=FUN)
	}
	fun
}

mzBin_postfun <- function(object, ans, ...) {
	if ( is.matter(ans) ) {
		data <- as(ans, "matter_matc")
	} else {
		data <- as.matrix(simplify2array(ans))
	}
	ref <- metadata(featureData(object))[["reference mz"]]
	if ( is.null(ref) )
		.stop("couldn't find reference mz")
	mcols <- MassDataFrame(mz=ref)
	metadata(mcols) <- metadata(featureData(object))
	object <- MSImagingExperiment(data,
		featureData=mcols,
		pixelData=pixelData(object),
		metadata=metadata(object),
		processing=processingData(object),
		centroided=TRUE)
	.message("binned to ", length(ref), " m/z per spectrum")
	object
}

mzBin_plotfun <- function(s2, s1, ...,
	main="Spectral binning", xlab="m/z", ylab="")
{
	mz <- mz(attr(s1, "mcols"))
	ref <- metadata(attr(s1, "mcols"))[["reference mz"]]
	if ( is.null(ref) )
		.stop("couldn't find reference mz")
	plot(range(mz), range(s2), main=main,
		xlab=xlab, ylab=ylab, type='n', ...)
	lines(mz, s1, col="gray", type='l')
	lines(ref, s2, col="red", type='h')
}
