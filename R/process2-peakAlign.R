
#### Align detected peaks to reference peaks ####
## ----------------------------------------------

setMethod("peakAlign", c("MSImagingExperiment", "missing"),
	function(object, tolerance = NA, units = c("ppm", "mz"), ...)
	{
		if ( is.na(tolerance) )
			tolerance <- .findMaxMassDiff(object, match.arg(units))
		tol <- switch(match.arg(units),
			ppm = c("relative" = unname(tolerance) * 1e-6),
			mz = c("absolute" = unname(tolerance)))
		if ( is.null(metadata(featureData(object))[["reference peaks"]]) ) {
			prefun <- peakAlign_prefun
		} else {
			prefun <- NULL
		}
		object <- process(object, label="peakAlign",
			kind="global", prefun=prefun,
			postfun=peakAlign_postfun, postargs=list(tol=tol),
			delay=getOption("Cardinal.delay"))
		object
	})

setMethod("peakAlign", c("MSImagingExperiment", "numeric"),
	function(object, ref, ...)
	{
		metadata(featureData(object))[["reference peaks"]] <- ref
		peakAlign(object, ...)
	})

setMethod("peakAlign", c("MSImagingExperiment", "character"),
	function(object, ref, ...)
	{
		col <- match.arg(ref, names(fData(object)))
		s <- fData(object)[[col]]
		maxs <- locmax(s, findLimits=TRUE)
		l1 <- attr(maxs, "lower")
		l2 <- attr(maxs, "upper")
		a <- binvec(s * mz(object), l1, l2, method="sum")
		b <- binvec(s, l1, l2, method="sum")
		ref <- a / b
		metadata(featureData(object))[["reference peaks"]] <- ref
		peakAlign(object, ...)
	})

peakAlign_prefun <- function(object, ..., BPPARAM) {
	s <- rowStats(spectra(object), stat="mean",
		chunks=getOption("Cardinal.numblocks"),
		verbose=getOption("Cardinal.verbose"),
		BPPARAM=BPPARAM)
	maxs <- locmax(s, findLimits=TRUE)
	l1 <- attr(maxs, "lower")
	l2 <- attr(maxs, "upper")
	a <- binvec(s * mz(object), l1, l2, method="sum")
	b <- binvec(s, l1, l2, method="sum")
	ref <- a / b
	metadata(featureData(object))[["reference peaks"]] <- ref
	object
}

peakAlign_postfun <- function(object, tol, ...) {
	if ( !is(object, "MSProcessedImagingExperiment") )
		object <- as(object, "MSProcessedImagingExperiment")
	ref <- metadata(featureData(object))[["reference peaks"]]
	if ( is.null(ref) )
		.stop("couldn't find reference peaks")
	mz(object) <- ref
	tolerance(object) <- tol
	combiner(object) <- "max"
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

