
#### Peak filtering methods ####
## ---------------------------

setMethod("peakFilter", "MSImageSet",
	function(object, method = "freq", ..., pixel, plot)
	{
		if ( !centroided(object) )
			.stop("peakFilter: 'object' is not centroided. Run 'peakAlign' on it first.")
		if ( !missing(pixel) )
			.warning("peakFilter: argument 'pixel' is depricated.")
		if ( !missing(plot) )
			.warning("peakFilter: argument 'plot' is depricated.")
		fun <- peakFilter.method(method)
		prochistory(processingData(object)) <- .history()
		.message("peakFilter: Using method = ", match.method(method))
		.time.start()
		feature <- featureApply(object, .fun=fun, ...,
			.use.names=FALSE, .simplify=TRUE)
		object <- object[feature,]
		.message("peakFilter: Done.")
		.time.stop()
		object
	})

peakFilter.method <- function(method, name.only=FALSE) {
	if ( is.character(method) || is.null(method) ) {
		options <- "freq"
		method <- match.method(method, options)
		if ( name.only )
			return(method)
		method <- switch(method,
			freq = peakFilter.freq,
			match.fun(method))
	}
	match.fun(method)
}

peakFilter.freq <- function(x, freq.min=length(x) / 100, ...) {
	sum(x > 0) > freq.min
}
