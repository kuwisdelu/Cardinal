
#### Bin spectra to reference m/z ####
## -----------------------------------

setMethod("mzBin", c("MSImagingExperiment", "numeric"),
	function(object, ref, tolerance = NA, units = c("ppm", "mz"), fun="sum", ...)
	{
		dots <- list(...)
		if ( "width" %in% names(dots) ) {
			tolerance <- dots$width
			.warning("'width' is deprecated\n",
				"Use 'tolerance' instead.")
		}
		width <- 2 * tolerance
		units <- match.arg(units)
		if ( is.na(width) )
			width <- 2 * .findMaxMassDiff(object, units)
		if ( length(ref) >= nrow(object) ) {
			.warning("new dimension [", length(ref), "] is greater ",
				"than current dimension [", nrow(object), "]")
		}
		metadata(featureData(object))[["reference mz"]] <- ref
		object <- process(object, fun=mzBin_fun,
			label="mzBin", kind="pixel",
			moreargs=list(ref=ref, width=width, units=units, FUN=fun),
			postfun=mzBin_postfun,
			postargs=list(width=width, units=units),
			plotfun=mzBin_plotfun,
			delay=getOption("Cardinal.delay"))
		object
	})

setMethod("mzBin", c("MSImagingExperiment", "missing"),
	function(object, from=min(mz(object)), to=max(mz(object)), by,
		resolution = NA, units = c("ppm", "mz"), fun=sum, ...)
	{
		units <- match.arg(units)
		if ( missing(by) )
			by <- switch(units, ppm=resolution * 2, mz=resolution)
		if ( is.na(by) )
			by <- 2 * .findMaxMassDiff(object, units)
		halfwidth <- by / 2
		ref <- switch(units,
			ppm = seq.ppm(from=from, to=to, ppm=halfwidth),
			mz = seq(from=from, to=to, by=2 * halfwidth))
		if ( length(ref) >= nrow(object) ) {
			.warning("new dimension [", length(ref), "] is greater ",
				"than current dimension [", nrow(object), "]")
		}
		mzBin(object, ref=ref, tolerance=halfwidth,
			units=units, fun=fun, ...)
	})

mzBin_fun <- function(x, ref, width, units, FUN) {
	mz <- mz(attr(x, "mcols"))
	halfwidth <- width / 2
	if ( units == "ppm" ) {
		lower <- ref - 1e-6 * halfwidth * ref
		upper <- ref + 1e-6 * halfwidth * ref
	} else {
		lower <- ref - halfwidth
		upper <- ref + halfwidth
	}
	lower <- 1L + findInterval(lower, mz, left.open=TRUE)
	upper <- findInterval(upper, mz, left.open=FALSE)
	binvec(x, lower, upper, method=FUN)
}

mzBin_postfun <- function(object, ans, width, units, ...) {
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
		centroided=NA)
	.message("binned to ", length(ref), " m/z bins per spectrum ",
		"(binwidth = ", width, " ", units, ")")
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
