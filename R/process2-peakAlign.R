
#### Align detected peaks to reference peaks ####
## ----------------------------------------------

setMethod("peakAlign", c("MSImagingExperiment", "missing"),
	function(object, tolerance = 200, units = c("ppm", "mz"), ...)
	{
		tol <- switch(match.arg(units),
			ppm = c("relative" = tolerance * 1e-6),
			mz = c("absolute" = tolerance))
		if ( is.null(metadata(featureData(object))[["reference peaks"]]) ) {
			prefun <- peakAlign_prefun
		} else {
			prefun <- NULL
		}
		postfun <- peakAlign_postfun(tol)
		object <- process(object, label="peakAlign",
			kind="global", prefun=prefun, postfun=postfun,
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
		s <- fData(object)[,col,drop=FALSE]
		ref <- mz(object)[localMaximaLogical(s)]
		metadata(featureData(object))[["reference peaks"]] <- ref
		peakAlign(object, ...)
	})

peakAlign_prefun <- function(object, ..., BPPARAM) {
	s <- summarize(object, .stat="mean",
		.by="feature", BPPARAM=BPPARAM)$mean
	ref <- mz(object)[localMaximaLogical(s)]
	metadata(featureData(object))[["reference peaks"]] <- ref
	object
}

peakAlign_postfun <- function(tol, ...) {
	fun <- function(object, ...) {
		if ( !is(object, "MSProcessedImagingExperiment") )
			object <- as(object, "MSProcessedImagingExperiment")
		ref <- metadata(featureData(object))[["reference peaks"]]
		if ( is.null(ref) )
			.stop("couldn't find reference peaks")
		mz(object) <- ref
		tolerance(object) <- tol
		combiner(object) <- "max"
		if ( !is.null(spectrumRepresentation(object)) )
			spectrumRepresentation(object) <- "centroid spectrum"
		centroided(object) <- TRUE
		object
	}
	fun
}

