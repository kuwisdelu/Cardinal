
#### Filter peaks based on frequency, etc. ####
## --------------------------------------------

setMethod("mzFilter", "MSImagingExperiment",
	function(object, ..., freq.min = NA, rm.zero = TRUE)
	{
		dots <- match.call(expand.dots=FALSE)$...
		expr <- eval(substitute(alist(...)))
		object <- process(object, label="mzFilter",
			kind="global", postfun=mzFilter_postfun,
			postargs=list(expr=expr, freq.min=freq.min, rm.zero=rm.zero),
			delay=getCardinalDelayProc())
		object
	})

setMethod("peakFilter", "MSImagingExperiment",
	function(object, ..., freq.min = 0.01, rm.zero = TRUE)
	{
		mzFilter(object, freq.min=freq.min, rm.zero=rm.zero, ...)
	})

mzFilter_postfun <- function(object, ..., expr, freq.min, rm.zero, BPPARAM) {
	keep <- rep_len(TRUE, nrow(object))
	if ( length(expr) > 0L ) {
		stats <- c("min", "max", "mean", "var", "nnzero")
	} else {
		stats <- c("nnzero")
	}
	summary <- rowStats(spectra(object), stat=stats, drop=FALSE,
		nchunks=getCardinalNumBlocks(),
		verbose=getCardinalVerbose(),
		BPPARAM=BPPARAM)
	summary[["count"]] <- summary[["nnzero"]]
	summary[["freq"]] <- summary[["count"]] / ncol(object)
	summary[["nnzero"]] <- NULL
	if ( isTRUE(rm.zero) ) {
		.message("dropping zero-intensity features")
		keep <- keep & summary$freq > 0
	}
	if ( !is.na(freq.min) ) {
		.message("applying freq.min = ", freq.min)
		keep <- keep & summary$freq >= freq.min
	}
	if ( length(expr) > 0L ) {
		envir <- as.env(summary)
		rules <- lapply(expr, function(e) {
			.message("applying rule: ", deparse(e))
			rule <- eval(e, envir=envir)
			if ( !is.logical(rule) )
				.stop("filter rules must be logical vectors")
			rule
		})
		keep <- keep & apply(do.call(cbind, rules), 1, all)
	}
	if ( anyNA(keep) )
		keep[is.na(keep)] <- FALSE
	if ( isTRUE(centroided(object)) ) {
		.message("removing ", sum(!keep), " peaks; ",
			"keeping ", sum(keep), " peaks")
	} else {
		.message("removing ", sum(!keep), " m/z features; ",
			"keeping ", sum(keep), " m/z features")
	}
	fData(object)[names(summary)] <- summary
	object[keep,]
}


