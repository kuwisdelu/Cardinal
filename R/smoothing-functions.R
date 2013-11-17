
#### functions for smoothing ####

smoothingFunction <- function(method) {
	if ( is.function(method) ) {
		return(method)
	} else if ( is.character(method) ) {
		if ( length(method) > 1 ) method <- method[[1]]
		switch(method,
			"none" = nullSmoothing,
			"gaussian" = gaussianSmoothing,
			"adaptive" = adaptiveSmoothing,
			match.fun(method)
		)
	} else {
		stop("could not find matching function for ", substitute(method))
	}
}

nullSmoothing <- function(x, ...) identity(x)

gaussianSmoothing <- function(x, sd=window/4, window=5, beta, max.intensity, .C=TRUE, ...) {
	x.drop <- as.numeric(x)
	r <- floor(window / 2)
	if ( .C ) {
		x.drop[is.na(x)] <- -1
		if ( missing(beta) ) beta <- matrix(1, nrow=length(x.drop), ncol=(2*r+1)^2)
		beta <- t(beta)
		x.new <- double(length(x))
		x.new <- .C("gaussian_filter", as.double(x.drop), as.double(x.new), as.integer(nrow(x)),
			as.integer(ncol(x)), as.double(sd), as.integer(r), as.double(beta))[[2]]
		x.new[is.na(x)] <- NA
	} else {
		coord <- expand.grid(lapply(dim(x), seq_len))
		coord <- coord[is.finite(x.drop),]
		x.finite <- x.drop[is.finite(x.drop)]
		positionArray <- generatePositionArray(coord+r, dim(x)+2*r)
		neighbors <- neighbors(coord+r, r, na.replace=TRUE)
		rs <- expand.grid((-r):r, (-r):r)
		alpha <- apply(rs, 1, gaussianKernel, sd=sd)
		x.new <- rep(NA, length(x.drop))
		if ( missing(beta) ) beta <- matrix(1, nrow=length(x.drop), ncol=(2*r+1)^2)
		beta <- beta[is.finite(x.drop),,drop=FALSE]
		x.new[is.finite(x.drop)] <- sapply(seq_along(x.finite), function(i) {
			gamma <- alpha * beta[i,]
			gamma <- gamma / sum(gamma)
			sum(gamma * x.finite[neighbors[i,]], na.rm=TRUE)
		} )
	}
	if ( missing(max.intensity) ) max.intensity <- max(x.new, na.rm=TRUE)
	x.new <- max.intensity * x.new / max(x.new, na.rm=TRUE)
	dim(x.new) <- dim(x)
	x.new
}

adaptiveSmoothing <- function(x, sd=window/4, window=5, max.intensity, .C=TRUE, ...) {
	x.drop <- as.numeric(x)
	r <- floor(window / 2)
	if ( .C ) {
		x.drop[is.na(x)] <- -1
		beta <- matrix(0, nrow=length(x), ncol=(2*r+1)^2)
		beta <- t(beta)
		x.new <- double(length(x))
		x.new <- .C("bilateral_filter", as.double(x.drop), as.double(x.new), as.integer(nrow(x)),
			as.integer(ncol(x)), as.double(sd), as.integer(r), as.double(beta))[[2]]
		x.new[is.na(x)] <- NA
		if ( missing(max.intensity) ) max.intensity <- max(x.new, na.rm=TRUE)
		x.new <- max.intensity * x.new / max(x.new, na.rm=TRUE)
		dim(x.new) <- dim(x)
	} else {
		coord <- expand.grid(lapply(dim(x), seq_len))
		coord <- coord[is.finite(x.drop),]
		neighbors <- neighbors(coord, r, na.replace=TRUE)
		beta <- matrix(0, ncol=(2*r + 1)^2, nrow=length(x))
		beta[is.finite(x.drop),] <- spatial.beta(matrix(x.drop[is.finite(x.drop)],
			nrow=1), neighbors)
		x.new <- gaussianSmoothing(x, sd=sd, window=window, beta=beta,
			max.intensity=max.intensity, .C=.C, ...)
	}
	x.new
}

#### helper functions ####

gaussianKernel <- function(x, sd) {
	x <- as.matrix(x)
	Sinv <- diag(1/sd^2, length(x))
	Sdet <- (sd^2)^length(x)
	a <- 1 / ((2*pi)^(length(x)/2) * sqrt(Sdet))
	a * exp((-1/2) * t(x) %*% Sinv %*% x)
}

