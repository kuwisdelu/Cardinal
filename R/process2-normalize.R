
#### Normalize spectra ####
## ------------------------

setMethod("normalize", "MSImagingExperiment",
	function(object, method = c("tic", "rms", "reference"), ...)
	{
		fun <- normalize.method2(method)
		object <- process(object, fun=fun, ...,
			label="normalize", kind="pixel",
			delay=TRUE)
		object
	})

normalize.method2 <- function(method) {
	if ( is.character(method) ) {
		method <- match.method(method,
			c("tic", "rms", "reference"))
		switch(method,
			tic = normalize.tic2,
			rms = normalize.rms,
			reference = normalize.reference,
			match.fun(method))
	} else {
		match.fun(method)
	}
}

normalize.tic2 <- normalize.tic

normalize.rms <- function(x, rms=1, ...) {
	qm <- sqrt(mean(x^2, na.rm=TRUE))
	if ( qm > 0 ) {
		xnew <- rms * x / qm
	} else {
		xnew <- rep(0, length(x))
	}
	replace(xnew, is.na(xnew), 0)
}

normalize.reference <- function(x, feature, scale=1, ...) {
	if ( missing(feature) ) {
		feature <- 1L
		.warning("missing 'feature', using feature = ", feature)
	}
	ref <- x[feature]
	if ( ref > 0 ) {
		xnew <- scale * x / ref
	} else {
		xnew <- rep(0, length(x))
	}
	replace(xnew, is.na(xnew), 0)
}
