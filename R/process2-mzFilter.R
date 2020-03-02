
#### Filter peaks based on frequency, etc. ####
## --------------------------------------------

setMethod("mzFilter", "MSImagingExperiment",
	function(object, ..., thresh.max = NA, freq.min = NA, rm.zero = TRUE)
	{
		expr <- eval(substitute(alist(...)))
		object <- process(object, label="mzFilter",
			kind="global", postfun=mzFilter_postfun,
			postargs=list(expr=expr, thresh.max=thresh.max,
						freq.min=freq.min, rm.zero=rm.zero),
			delay=getOption("Cardinal.delay"))
		object
	})

setMethod("peakFilter", "MSImagingExperiment",
	function(object, ..., thresh.max = NA, freq.min = 0.01, rm.zero = TRUE)
	{
		mzFilter(object, thresh.max=thresh.max, freq.min=freq.min, rm.zero=rm.zero, ...)
	})

mzFilter_postfun <- function(object, ..., expr, thresh.max, freq.min, rm.zero, BPPARAM) {
	do_freq <- isTRUE(freq.min > 0)
	do_expr <- length(expr) > 0L
	do_thresh <- isTRUE(thresh.max > 0)
	keep <- rep_len(TRUE, nrow(object))
	if ( do_freq || (rm.zero && !(do_expr || do_thresh)) ) {
		summary1 <- summarize(object, .stat=c(count="sum", freq="mean"),
							.tform=function(x) x > 0, .by="feature",
							.as="DataFrame", BPPARAM=BPPARAM)
		if ( rm.zero ) {
			.message("removing zero-intensity features")
			keep <- keep & summary1$count > 0
		}
		.message("applying freq.min = ", freq.min)
		keep <- keep & summary1$freq > freq.min
	}
	if ( do_expr || do_thresh || (rm.zero && !do_freq) ) {
		if ( do_expr ) {
			stats <- c("min", "max", "mean", "var")
		} else {
			stats <- c("max", "mean")
		}
		summary2 <- summarize(object, .stat=stats, .by="feature",
							.as="DataFrame", BPPARAM=BPPARAM)
		if ( do_freq ) {
			.message("combining feature summaries")
			summary1 <- cbind(summary1, summary2)
		} else {
			summary1 <- summary2
		}
		if ( rm.zero && !do_freq ) {
			.message("removing zero-intensity features")
			keep <- keep & summary1$max > 0
		}
		if ( do_thresh ) {
			.message("applying thresh.max = ", thresh.max)
			keep <- keep & (summary1$max > thresh.max * max(summary1$mean))
		}
		if ( length(expr) > 0L ) {
			e <- as.env(summary1)
			rules <- lapply(expr, function(a) {
				.message("applying rule = ", deparse(a))
				rule <- eval(a, envir=e)
				if ( !is.logical(rule) )
					.stop("filter rules must be logical vectors")
				rule
			})
			keep <- keep & rowSums(do.call(cbind, rules)) != 0
		}
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
	fData(object)[names(summary1)] <- summary1
	object[keep,]
}
