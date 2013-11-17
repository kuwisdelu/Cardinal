
#### functions for denoising and smoothing ####

filterFunction <- function(method) {
	if ( is.function(method) ) {
		return(method)
	} else if ( is.character(method) ) {
		if ( length(method) > 1 ) method <- method[[1]]
		switch(method,
			"none" = nullFilter,
			"gaussian" = gaussianFilter,
			"sgolay" = savitskyGolayFilter,
			"kaiser" = kaiserFilter,
			"ma" = movingAverageFilter,
			match.fun(method)
		)
	} else {
		stop("could not find matching function for ", substitute(method))
	}
}

nullFilter <- function(x, t, ...) identity(x)

gaussianFilter <- function(x, t, sd=window/4, window=5, ...) {
	halfWindow <- floor(window / 2)
	coef <- dnorm(-halfWindow:halfWindow, sd=sd)
	movingAverageFilter(x, t, coef=coef, ...)
}

savitskyGolayFilter <- function(x, t, order=3, window=5, ...) {
	window <- ifelse(window %% 2 == 0, window + 1, window)
	if ( window >= order ) order <- window - 1
	xout <- sgolayfilt(x, p=order, n=window)
	attributes(xout) <- NULL
	xout
}

kaiserFilter <- function(x, t, beta=1, window=5, ...) {
	coef <- kaiser(n=window, beta=beta)
	movingAverageFilter(x, t, coef=coef, ...)
}

movingAverageFilter <- function(x, t, coef=rep(1,window), window=5, pad=TRUE, ...) {
	coef <- coef / sum(coef)
	window <- length(coef)
	halfWindow <- floor(window / 2)
	if ( pad ) x <- c(rep(x[1], halfWindow), x, rep(x[length(x)], halfWindow))
	xout <- stats::filter(x, filter=coef)
	attributes(xout) <- NULL
	if ( pad ) xout <- xout[(halfWindow + 1):(length(x) - halfWindow)]
	xout
}
