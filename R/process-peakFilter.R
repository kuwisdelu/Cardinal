
#### Peak filtering methods ####
## ---------------------------

setMethod("peakFilter", "MSImageSet",
	function(object, method = "freq", ...)
	{
		if ( !centroided(object) )
			.stop("peakFilter: 'object' is not centroided. Run 'peakAlign' on it first.")
		fun <- peakFilter.method(method)
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

peakFilter.freq <- function(x, freq.min=0.01, ...) {
	if ( freq.min >= 1 ) {
		warning("freq.min >= 1 detected; assuming count")
		freq.min <- freq.min / length(x)
	}
	(sum(x > 0) / length(x)) > freq.min
}
