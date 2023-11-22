
#### Align detected peaks to reference peaks ####
## ----------------------------------------------

setMethod("peakAlign", c("MSImagingExperiment", "missing"),
	function(object, tolerance = NA, units = c("ppm", "mz"), ...)
	{
		units <- match.arg(units)
		if ( is.na(tolerance) )
			tolerance <- .estimateMassResolution(mz(object), units)
		tol <- switch(match.arg(units),
			ppm = c("relative" = unname(tolerance) * 1e-6),
			mz = c("absolute" = unname(tolerance)))
		if ( is.null(metadata(featureData(object))[["peakAlign_ref"]]) ) {
			prefun <- peakAlign_prefun
		} else {
			prefun <- NULL
		}
		object <- process(object, label="peakAlign",
			kind="global", prefun=prefun,
			postfun=peakAlign_postfun, postargs=list(tol=tol),
			delay=getCardinalDelayProc())
		object
	})

setMethod("peakAlign", c("MSImagingExperiment", "numeric"),
	function(object, ref, ...)
	{
		ref <- sort(unique(ref))
		metadata(featureData(object))[["peakAlign_ref"]] <- ref
		peakAlign(object, ...)
	})

setMethod("peakAlign", c("MSImagingExperiment", "character"),
	function(object, ref, ...)
	{
		col <- match.arg(ref, names(fData(object)))
		s <- fData(object)[[col]]
		maxs <- findpeaks(s, relheight=0)
		l1 <- attr(maxs, "left_bounds")
		l2 <- attr(maxs, "right_bounds")
		a <- binvec(s * mz(object), l1, l2, stat="sum")
		b <- binvec(s, l1, l2, stat="sum")
		ref <- sort(unique(a / b))
		metadata(featureData(object))[["peakAlign_ref"]] <- ref
		peakAlign(object, ...)
	})

peakAlign_prefun <- function(object, ..., BPPARAM) {
	s <- rowStats(spectra(object), stat="mean",
		nchunks=getCardinalNumBlocks(),
		verbose=getCardinalVerbose(),
		BPPARAM=BPPARAM)
	maxs <- findpeaks(s)
	l1 <- attr(maxs, "left_bounds")
	l2 <- attr(maxs, "right_bounds")
	a <- binvec(s * mz(object), l1, l2, stat="sum")
	b <- binvec(s, l1, l2, stat="sum")
	ref <- sort(unique(a / b))
	metadata(featureData(object))[["peakAlign_ref"]] <- ref
	object
}

peakAlign_postfun <- function(object, tol, ...) {
	if ( !is(object, "MSProcessedImagingExperiment") )
		object <- as(object, "MSProcessedImagingExperiment")
	ref <- metadata(featureData(object))[["peakAlign_ref"]]
	if ( is.null(ref) )
		.stop("couldn't find reference peaks")
	mz(object) <- ref
	tolerance(object) <- tol
	sampler(object) <- "max"
	tol <- switch(names(tol),
		relative = c(ppm = unname(tol) / 1e-6),
		absolute = c(mz = unname(tol)))
	resolution(featureData(object)) <- 2 * tol
	.message("aligned to ", length(ref), " reference peaks ",
		"(tol = ", tol, " ",  names(tol), ")")
	if ( !is.null(spectrumRepresentation(object)) )
		spectrumRepresentation(object) <- "centroid spectrum"
	centroided(object) <- TRUE
	object
}

