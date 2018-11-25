
#### Contrast enhancement for an N-dimensional image ####

contrast.enhance.method <- function(method) {
	if ( is.character(method) ) {
		method <- match.method(method, c("none", "suppression", "histogram"))
		method <- switch(method,
			none = identity,
			suppression = contrast.enhance.suppression,
			histogram = contrast.enhance.histogram,
			match.fun(method))
	}
	match.fun(method)
}

contrast.enhance.suppression <- function(x, top.percent=0.01, ...) {
	if ( all(is.na(x)) ) return(x)
	max.intensity <- max(x, na.rm=TRUE)
	cutoff <- quantile(x, 1 - top.percent, na.rm=TRUE)
	x[x > cutoff] <- cutoff
	x
}

contrast.enhance.histogram <- function(x, blocks=100, ...) {
	if ( all(is.na(x)) ) return(x)
	breaks <- unique(quantile(x, seq(from=0, to=1, length.out=blocks), na.rm=TRUE))
	x.cut <- cut(x, breaks, include.lowest=TRUE)
	x.new <- as.numeric(x.cut) / length(levels(x.cut))
	top.x <- tail(breaks[-length(breaks)], 1)
	scale <- mean(x, na.rm=TRUE) / mean(x.new, na.rm=TRUE)
	x.new <- scale * x.new
	x.new + min(x, na.rm=TRUE)
	dim(x.new) <- dim(x)
	x.new
}

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
	x.new <- .Call("gaussianFilter", x, r, sd, PACKAGE="Cardinal")
	x.new <- max(x, na.rm=TRUE) * x.new / max(x.new, na.rm=TRUE)
	x.new
}

smooth.image.adaptive <- function(x, window=3, ...) {
	if ( all(is.na(x)) ) return(x)
	r <- floor(window / 2)
	sd <- window / 4
	x.new <- .Call("bilateralFilter", x, r, sd, PACKAGE="Cardinal")
	x.new <- max(x, na.rm=TRUE) * x.new / max(x.new, na.rm=TRUE)
	x.new
}

#### Normalization for a 2D image ####

normalize.image.method <- function(method) {
	if ( is.character(method) ) {
		method <- match.method(method, c("none", "linear"))
		method <- switch(method,
			none = identity,
			linear = normalize.image.linear,
			match.fun(method))
	}
	match.fun(method)
}

normalize.image.linear <- function(x, range = c(0,1), ...) {
	if ( all(is.na(x)) ) return(x)
	oldmin <- min(x, na.rm=TRUE)
	oldmax <- max(x, na.rm=TRUE)
	min <- range[1]
	max <- range[2]
	((x - oldmin) * (max - min) / (oldmax - oldmin)) + min
}

#### Interpolationg for a 2D image ####

interpolate.bilinear <- function(x, xres=2, size=xres*dim(x), ...) {
	obj <- list(x=1:dim(x)[[1]], y=1:dim(x)[[2]], z=x)
	grid.list <- list(x=seq(1, dim(x)[[1]], length.out=size[[1]]),
		y=seq(1, dim(x)[[2]], length.out=size[[2]]))
	interp.surface.grid(obj, grid.list)$z
}

# copied from 'fields' package because 'maps' package is broken on Windows
interp.surface.grid <- function (obj, grid.list) {
	x <- grid.list$x
	y <- grid.list$y
	M <- length(x)
	N <- length(y)
	out <- matrix(NA, nrow = M, ncol = N)
	for (i in 1:M) {
	    out[i, ] <- interp.surface(obj, cbind(rep(x[i], N), y))
	}
	list(x = x, y = y, z = out)
}

# copied from 'fields' package because 'maps' package is broken on Windows
interp.surface <- function (obj, loc)  {
	x <- obj$x
	y <- obj$y
	z <- obj$z
	nx <- length(x)
	ny <- length(y)
	lx <- approx(x, 1:nx, loc[, 1])$y
	ly <- approx(y, 1:ny, loc[, 2])$y
	lx1 <- floor(lx)
	ly1 <- floor(ly)
	ex <- lx - lx1
	ey <- ly - ly1
	ex[lx1 == nx] <- 1
	ey[ly1 == ny] <- 1
	lx1[lx1 == nx] <- nx - 1
	ly1[ly1 == ny] <- ny - 1
	return(z[cbind(lx1, ly1)] * (1 - ex) * (1 - ey) + z[cbind(lx1 + 
	    1, ly1)] * ex * (1 - ey) + z[cbind(lx1, ly1 + 1)] * (1 - 
	    ex) * ey + z[cbind(lx1 + 1, ly1 + 1)] * ex * ey)
}

