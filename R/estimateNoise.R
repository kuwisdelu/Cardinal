
# Estimate noise in a signal

.estimateNoiseSimple <- function(x, blocks=100) {
	t <- seq_along(x)
	xint <- blocks(x, blocks=blocks)
	kurt <- sapply(xint, kurtosis) - 3
	noise <- mean(sapply(xint, sd)[kurt < 1], na.rm=TRUE)
	noise <- rep(noise, length(x))
	noise
}

.estimateNoiseAdaptive <- function(x, blocks=100, spar=1) {
	t <- seq_along(x)
	xint <- blocks(x, blocks=blocks)
	xlen <- sapply(xint, length)
	noise <- sapply(xint, sd)
	noise <- unlist(mapply(function(ns, ln) rep(ns, ln), noise, xlen))
	cutoff <- smooth.spline(x=t, y=noise, spar=1)$y
	noise <- interp1(t[noise <= cutoff], noise[noise <= cutoff], xi=t,
		method="linear", extrap=median(noise, na.rm=TRUE))
	noise
}
