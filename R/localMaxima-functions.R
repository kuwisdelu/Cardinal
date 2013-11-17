
#### functions for finding local maxima ####

localMaxima <- function(x, span=5, .C=TRUE, ...) {
	which(localMaximaLogical(x, span=span, .C=.C, ...))
}

localMaximaLogical <- function(x, span=5, .C=TRUE, ...) {
	halfspan <- floor(span / 2)
	if ( .C ) {
		is.max <- integer(length(x))
		is.max <- .C("localMaxima", as.double(x), as.integer(is.max),
			as.integer(length(x)), as.integer(halfspan))[[2]]
		is.max <- as.logical(is.max)
	} else {
		is.max <- sapply(seq(from=1+halfspan, to=length(x)-halfspan, by=1), function(i) {
			which.max(x[(i-halfspan):(i+halfspan)]) == halfspan + 1
		} )
		is.max <- c(rep(FALSE, halfspan), is.max, rep(FALSE, halfspan))
	}
	return(is.max)
}

nearestLocalMaxima <- function(x, t, tout, ...) {
	locmax <- localMaximaLogical(x, ...)
	locmax <- t[locmax]
	locmax <- c(min(t), locmax, max(t))
	limits <- sapply(tout, function(i) {
		lower <- which(diff(sign(i - locmax)) < 0)
		if ( length(lower) > 1 ) {
			lower <- lower[[1]]
			warning("nearest local extrema are not unique")
		} else if ( length(lower) < 1 ) {
			stop("no local nearest local extrema")
		}
		upper <- lower + 1
		return(c(locmax[lower], locmax[upper]))
	} )
	if ( length(limits) > 1 ) {
		return(data.frame(tlower=limits[1,], tupper=limits[2,]))
	} else {
		return(data.frame())
	}
}

localMaximaWithinBounds <- function(x, lbound, ubound) {
	mapply(function(l, u) {
		l + which.max(x[l:u]) - 1
	}, lbound, ubound)
}
