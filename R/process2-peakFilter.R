
#### Filter peaks based on frequency, etc. ####
## --------------------------------------------

setMethod("peakFilter", "MSImagingExperiment",
	function(object, freq.min = 0.01, ...)
	{
		expr <- eval(substitute(alist(...)))
		postfun <- peakFilter_postfun(expr, freq.min)
		object <- process(object, label="peakFilter",
			kind="global", postfun=postfun, delay=TRUE)
		object
	})

peakFilter_postfun <- function(expr, freq.min) {
	fun <- function(object, ..., BPPARAM) {
		summary1 <- summarize(object, .stat=c(count="sum", freq="mean"),
			.tform=function(x) x > 0, .by="feature", BPPARAM=BPPARAM)
		keep <- summary1$freq > freq.min
		if ( length(expr) > 1L ) {
			summary2 <- summarize(object,
				.stat=c("min", "max", "mean", "sum", "sd", "var"),
				.by="feature", BPPARAM=BPPARAM)
			summary1 <- cbind(summary1, summary2)
			e <- as.env(summary1)
			rules <- lapply(expr, function(a) {
				rule <- eval(a, envir=e)
				if ( !is.logical(rule) )
					.stop("filter rules must be logical vectors")
				rule
			})
			keep <- keep & rowSums(do.call(cbind, rules)) != 0
		}
		.message("removing ", sum(!keep), " peaks; ",
			"keeping ", sum(keep), " peaks")
		object[keep,]
	}
	fun
}
