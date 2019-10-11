
#### Bin spectra to reference peaks ####
## ------------------------------------

setMethod("peakBin", c("MSImagingExperiment", "numeric"),
	function(object, ref, type=c("height", "area"),
		tolerance = NA, units = c("ppm", "mz"), ...)
	{
		if ( is.na(tolerance) )
			tolerance <- 2 * .findMaxMassDiff(object, match.arg(units))
		tol <- switch(match.arg(units),
			ppm = c("relative" = unname(tolerance) * 1e-6),
			mz = c("absolute" = unname(tolerance)))
		tol.ref <- switch(names(tol),
			relative = "key",
			absolute = "none")
		mz <- mz(object)
		peaks <- bsearch(ref, mz, tol=tol, tol.ref=tol.ref)
		peaks <- peaks[!is.na(peaks)]
		type <- match.arg(type)
		fun <- peakBin_fun(type, tol, mz, peaks)
		postfun <- peakBin_postfun(tol)
		metadata(featureData(object))[["reference peaks"]] <- ref
		object <- process(object, fun=fun,
			label="peakBin", kind="pixel",
			moreargs=list(...),
			postfun=postfun,
			plotfun=peakBin_plotfun,
			delay=getOption("Cardinal.delay"))
		object
	})

setMethod("peakBin", c("MSImagingExperiment", "missing"),
	function(object, type=c("height", "area"),
		tolerance = NA, units = c("ppm", "mz"), ...)
	{
		if ( is.na(tolerance) )
			tolerance <- 2 * .findMaxMassDiff(object, match.arg(units))
		tol <- switch(match.arg(units),
			ppm = c("relative" = unname(tolerance) * 1e-6),
			mz = c("absolute" = unname(tolerance)))
		type <- match.arg(type)
		fun <- peakBin_fun(type, tol, NULL, NULL)
		postfun <- peakBin_postfun(tol)
		object <- process(object, fun=fun, ...,
			label="peakBin", kind="pixel",
			prefun=peakBin_prefun,
			postfun=postfun,
			plotfun=peakBin_plotfun,
			delay=getOption("Cardinal.delay"))
		object
	})

peakBin_fun <- function(type, tol, mz, peaks) {
	fun <- function(x, ...) {
		mzs <- mz(attr(x, "mcols"))
		if ( !identical(mz, mzs) ) {
			ref <- metadata(attr(x, "mcols"))[["reference peaks"]]
			if ( is.null(ref) )
				.stop("couldn't find reference peaks")
			tol.ref <- switch(names(tol),
				relative = "key",
				absolute = "none")
			peaks <- bsearch(ref, mzs, tol=tol, tol.ref=tol.ref)
			peaks <- peaks[!is.na(peaks)]
		}
		f <- switch(type, height=max, area=sum)
		bounds <- nearestLocalMaxima(-x, seq_along(x), peaks)
		peaks <- bin_vector(x, bins=bounds, fun=f)
		attr(peaks, "bins") <- bounds
		peaks
	}
	fun
}

peakBin_prefun <- function(object, ..., BPPARAM) {
	s <- summarize(object, .stat="mean",
		.by="feature", BPPARAM=BPPARAM)$mean
	ref <- mz(object)[localMaxima(s)]
	metadata(featureData(object))[["reference peaks"]] <- ref
	object
}

peakBin_postfun <- function(tol, ...) {
	fun <- function(object, ans, ...) {
		if ( is.matter(ans) ) {
			data <- as(ans, "matter_matc")
		} else {
			data <- as.matrix(simplify2array(ans))
		}
		ref <- metadata(featureData(object))[["reference peaks"]]
		if ( is.null(ref) )
			.stop("couldn't find reference peaks")
		mcols <- MassDataFrame(mz=ref)
		res <- switch(names(tol),
				relative = c(ppm = unname(tol) / 1e-6),
				absolute = c(mz = 2 * unname(tol)))
		metadata(mcols) <- metadata(featureData(object))
		object <- MSImagingExperiment(data,
			featureData=mcols,
			pixelData=pixelData(object),
			metadata=metadata(object),
			processing=processingData(object),
			centroided=TRUE)
		res <- switch(names(tol),
			relative = c(ppm = unname(tol) / 1e-6),
			absolute = c(mz = 2 * unname(tol)))
		resolution(featureData(object)) <- res
		if ( !is.null(spectrumRepresentation(object)) )
			spectrumRepresentation(object) <- "centroid spectrum"
		.message("binned to ", length(ref), " reference peaks ",
			"(", names(tol), " tol = ", tol, ")")
		object
	}
	fun
}

peakBin_plotfun <- function(s2, s1, ...,
	main="Peak binning", xlab="m/z", ylab="")
{
	mz <- mz(attr(s1, "mcols"))
	ref <- metadata(attr(s1, "mcols"))[["reference peaks"]]
	if ( is.null(ref) )
		.stop("couldn't find reference peaks")
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
