
#### functions for noise estimate ####

estimateNoiseFunction <- function(method) {
	if ( is.function(method) ) {
		return(method)
	} else if ( is.character(method) ) {
		if ( length(method) > 1 ) method <- method[[1]]
		switch(method,
			"sd" = estimateNoiseSD,
			"mad" = estimateNoiseMAD,
			"adaptive-sd" = estimateNoiseAdaptiveSD,
			"adaptive-mad" = estimateNoiseAdaptiveMAD,
			"limpic" = estimateNoiseLIMPIC,
			"supersmoother" = estimateNoiseSuperSmoother,
			match.fun(method)
		)
	} else {
		stop("could not find matching function for ", substitute(method))
	}
}

estimateNoiseSimple <- function(x, t, devfun=sd, kurt.max=1, blocks=100, ...) {
	fun <- match.fun(devfun)
	# identify blocks with little signal
	xint <- intervals(x, blocks=blocks)
	kurt <- sapply(xint, kurtosis) - 3
	# estimate the mean noise level
	noise <- mean(sapply(xint, fun)[kurt < kurt.max], na.rm=TRUE)
	rep(noise, length(x))
}

estimateNoiseSD <- function(x, t, ...) {
	estimateNoiseSimple(x, t, devfun=sd, ...)
}

estimateNoiseMAD <- function(x, t, ...) {
	estimateNoiseSimple(x, t, devfun=mad, ...)
}

estimateNoiseAdaptive <- function(x, t, devfun=sd, spar=1, interp1=c("linear",
	"nearest", "pchip", "cubic", "spline"), blocks=100, ...) {
	fun <- match.fun(devfun)
	# identify blocks with little signal
	xint <- intervals(x, blocks=blocks)
	xlen <- sapply(xint, length)
	# estimate the noise level in each block
	noise <- sapply(xint, fun)
	noise <- unlist(sapply(seq_along(noise), function(i) rep(noise[i], xlen[i])))
	# smooth the noise
	cutoff <- smooth.spline(x=t, y=noise, spar=spar)$y
	method = match.arg(interp1)
	interp1(t[noise <= cutoff], noise[noise <= cutoff], xi=t, method=method,
		extrap=median(noise, na.rm=TRUE))
}

estimateNoiseAdaptiveSD <- function(x, t, ...) {
	estimateNoiseAdaptive(x, t, devfun=sd)
}

estimateNoiseAdaptiveMAD <- function(x, t, ...) {
	estimateNoiseAdaptive(x, t, devfun=mad)
}

estimateNoiseLIMPIC <- function(x, t, kurt.max=1, blocks=100, ...) {
	if ( missing(t) ) t <- seq_along(x)
	# identify flat blocks of the signal
	xint <- intervals(x, blocks=blocks)
	kurt <- sapply(xint, kurtosis) - 3
	mean <- sapply(xint, mean)
	is.flat <- kurt < kurt.max & mean < mean(x)
	# interpolate non-uniform noise level
	noise.val <- sapply(xint, sd)[is.flat]
	noise.val <- c(median(noise.val), noise.val, median(noise.val))
	noise.idx <- floor(seq(from=blocks/2, to=length(x) - blocks/2,
		length.out=length(xint)))[is.flat]
	noise.idx <- c(1, noise.idx, length(x))
	noise <- interp1(x=t[noise.idx], y=noise.val, xi=t)
	noise
}

estimateNoiseSuperSmoother <- function(x, t, ...) {
	supsmu(t, x)$y
}


