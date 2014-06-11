
#### functions for finding local maxima ####

localMaxima <- function(x, window=5, .C=TRUE, ...) {
	which(localMaximaLogical(x, window=window, .C=.C, ...))
}

localMaximaLogical <- function(x, window=5, .C=TRUE, ...) {
	halfWindow <- floor(window / 2)
	if ( .C ) {
		is.max <- integer(length(x))
		is.max <- as.logical(.C("localMaxima", as.double(x), as.integer(is.max),
			as.integer(length(x)), as.integer(halfWindow))[[2]])
	} else {
		is.max <- sapply(seq(from=1+halfWindow, to=length(x)-halfWindow, by=1),
			function(i) which.max(x[(i-halfWindow):(i+halfWindow)]) == halfWindow + 1)
		is.max <- c(rep(FALSE, halfWindow), is.max, rep(FALSE, halfWindow))
	}
	is.max
}

nearestLocalMaxima <- function(x, t, tout, ...) {
	locmax <- localMaximaLogical(x, ...)
	locmax <- t[locmax]
	locmax <- unique(c(min(t), locmax, max(t)))
	limits <- sapply(tout, function(ti) {
		lower <- which(diff(sign(ti - locmax)) < 0)
		if ( length(lower) > 1 )
			lower <- lower[[1]]
		upper <- lower + 1
		c(locmax[lower], locmax[upper])
	})
	list(lbound=limits[1,], ubound=limits[2,])
}

