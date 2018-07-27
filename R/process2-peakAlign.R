
#### Peak alignment methods ####
## ---------------------------

setMethod("peakAlign", c("MSImagingExperiment", "missing"),
	function(object, tolerance = 200, units = c("ppm", "mz"),
			type=c("height", "area"), ...)
	{
		tol <- switch(match.arg(units),
			ppm = c("relative" = tolerance * 1e-6),
			mz = c("absolute" = tolerance))
		type <- match.arg(type)
		if ( is.null(metadata(object)[["reference peaks"]]) ) {
			prefun <- peakAlign_prefun
		} else {
			prefun <- NULL
		}
		postfun <- peakAlign_postfun(tol, type)
		object <- process(object, label="peakAlign",
			kind="global", prefun=prefun, postfun=postfun,
			delay=TRUE)
		object
	})

setMethod("peakAlign", c("MSImagingExperiment", "numeric"),
	function(object, ref, ...)
	{
		metadata(object)[["reference peaks"]] <- ref
		peakAlign(object, ...)
	})

setMethod("peakAlign", c("MSImagingExperiment", "character"),
	function(object, ref, ...)
	{
		col <- match.arg(ref, names(fData(object)))
		s <- fData(object)[,col,drop=FALSE]
		ref <- mz(object)[localMaximaLogical(s)]
		metadata(object)[["reference peaks"]] <- ref
		peakAlign(object, ...)
	})

peakAlign_prefun <- function(object, ..., BPPARAM) {
	s <- summarize(object, .stat="mean",
		.by="feature", BPPARAM=BPPARAM)$mean
	ref <- mz(object)[localMaximaLogical(s)]
	metadata(object)[["reference peaks"]] <- ref
	object
}

peakAlign_postfun <- function(tol, type, ...) {
	fun <- function(object, ...) {
		if ( !is(object, "MSProcessedImagingExperiment") )
			object <- as(object, "MSProcessedImagingExperiment")
		ref <- metadata(object)[["reference peaks"]]
		mz(object) <- ref
		tolerance(object) <- tol
		combiner(object) <- switch(type,
			height="max", area="sum")
		if ( !is.null(spectrumRepresentation(object)) )
			spectrumRepresentation(object) <- "centroid spectrum"
		centroided(object) <- TRUE
		object
	}
	fun
}

