
#### Filter peaks based on frequency, etc. ####
## --------------------------------------------

setMethod("peakFilter", "MSImagingExperiment",
	function(object, ..., freq.min = 0.01, thresh.max = 0)
	{
		expr <- eval(substitute(alist(...)))
		postfun <- peakFilter_postfun(expr, freq.min, thresh.max)
		object <- process(object, label="peakFilter",
			kind="global", postfun=postfun,
			delay=getOption("Cardinal.delay"))
		object
	})

setMethod("mzFilter", "MSImagingExperiment",
	function(object, ..., freq.min = 0, thresh.max = 0.01)
	{
		peakFilter(object, freq.min=freq.min, thresh.max=thresh.max, ...)
	})

peakFilter_postfun <- function(expr, freq.min, thresh.max) {
	fun <- function(object, ..., BPPARAM) {
		if ( freq.min > 0 ) {
			summary1 <- summarize(object, .stat=c(count="sum", freq="mean"),
				.tform=function(x) x > 0, .by="feature", BPPARAM=BPPARAM)
			.message("applying freq.min = ", freq.min)
			keep <- summary1$freq > freq.min
		} else {
			keep <- rep_len(TRUE, nrow(object))
		}
		if ( length(expr) > 0L || thresh.max > 0 ) {
			summary2 <- summarize(object,
				.stat=c("min", "max", "mean", "sum", "sd", "var"),
				.by="feature", BPPARAM=BPPARAM)
			if ( !all(keep) )
				summary2 <- cbind(summary1, summary2)
			if ( thresh.max > 0 ) {
				.message("applying thresh.max = ", thresh.max)
				keep <- keep & (summary2$mean > thresh.max * max(summary2$mean))
			}
			if ( length(expr) > 0L ) {
				e <- as.env(summary2)
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
		.message("removing ", sum(!keep), " peaks; ",
			"keeping ", sum(keep), " peaks")
		object[keep,]
	}
	fun
}
