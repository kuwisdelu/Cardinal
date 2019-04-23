
#### Normalize spectra ####
## ------------------------

setMethod("normalize", "SparseImagingExperiment",
	function(object, method = c("tic", "rms", "reference"), ...)
	{
		if ( is.character(method) && method[1] == "reference" ) {
			dots <- match.call(expand.dots=FALSE)$...
			if ( !"feature" %in% names(dots) )
				.stop("feature must be specified for method = 'reference'")
		}
		fun <- normalize.method2(method)
		object <- process(object, fun=fun, ...,
			label="normalize", kind="pixel",
			plotfun=normalize_plotfun,
			delay=getOption("Cardinal.delay"))
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

normalize_plotfun <- function(s2, s1, ...,
	main="Normalization", xlab="", ylab="")
{
	mcols <- attr(s1, "mcols")
	if ( is(mcols, "MassDataFrame") ) {
		x <- mz(mcols)
		if ( missing(xlab) )
			xlab <- "m/z"
	} else {
		x <- seq_along(s2)
	}
	plot(range(x), range(s2), main=main,
		xlab=xlab, ylab=ylab, type='n', ...)
	lines(x, s1, col="gray", type='l')
	lines(x, s2, col="black", type='l')
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
