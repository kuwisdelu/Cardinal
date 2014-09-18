
#### Smoothing for a 2D image ####

smooth.image.method <- function(method) {
	if ( is.character(method) ) {
		method <- match.method(method, c("none", "gaussian", "adaptive"))
		method <- switch(method,
			none = identity,
			gaussian = smooth.image.gaussian,
			adaptive = smooth.image.adaptive,
			match.fun(method))
	}
	match.fun(method)
}

smooth.image.gaussian <- function(x, window=3, ...) {
	if ( all(is.na(x)) ) return(x)
	r <- floor(window / 2)
	sd <- window / 4
	beta <- matrix(1, nrow=((2 * r) + 1)^2, ncol=length(x))
	missing <- is.na(x)
	x[missing] <- -1
	x.new <- double(length(x))
	x.new <- .C("gaussian_filter", as.double(x), as.double(x.new),
		as.integer(nrow(x)), as.integer(ncol(x)), as.double(sd),
		as.integer(r), as.double(beta))[[2]]
	x.new[missing] <- NA
	x.new <- max(x, na.rm=TRUE) * x.new / max(x.new, na.rm=TRUE)
	dim(x.new) <- dim(x)
	x.new
}

smooth.image.adaptive <- function(x, window=3, ...) {
	if ( all(is.na(x)) ) return(x)
	r <- floor(window / 2)
	sd <- window / 4
	beta <- matrix(1, nrow=((2 * r) + 1)^2, ncol=length(x))
	missing <- is.na(x)
	x[missing] <- -1
	x.new <- numeric(length(x))
	x.new <- .C("bilateral_filter", as.double(x), as.double(x.new),
		as.integer(nrow(x)), as.integer(ncol(x)), as.double(sd),
		as.integer(r), as.double(beta))[[2]]
	x.new[missing] <- NA
	x.new <- max(x, na.rm=TRUE) * x.new / max(x.new, na.rm=TRUE)
	dim(x.new) <- dim(x)
	x.new
}
