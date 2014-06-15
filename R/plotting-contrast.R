
#### Contrast enhancement for an N-dimensional image ####

contrast.enhance.method <- function(method) {
	if ( is.character(method) ) {
		method <- match.method(method, c("suppression", "histogram"))
		method <- switch(method,
			suppression = contrast.enhance.suppression,
			histogram = contrast.enhance.histogram,
			match.fun(method))
	}
	match.fun(method)
}

contrast.enhance.suppression <- function(x, top.percent=0.01, ...) {
	max.intensity <- max(x, na.rm=TRUE)
	cutoff <- quantile(x, 1 - top.percent, na.rm=TRUE)
	x[x > cutoff] <- cutoff
	max.intensity * x / max(x, na.rm=TRUE)
}

contrast.enhance.histogram <- function(x, blocks=100, ...) {
	breaks <- unique(quantile(x, seq(from=0, to=1, length.out=blocks), na.rm=TRUE))
	x.cut <- cut(x, breaks, include.lowest=TRUE)
	x.new <- as.numeric(x.cut) / length(levels(x.cut))
	top.x <- tail(breaks[-length(breaks)], 1)
	min.intensity <- min(x, na.rm=TRUE)
	max.intensity <- max(x, na.rm=TRUE)
	x.new <- (max.intensity - min.intensity) * x.new
	x.new + min.intensity
	dim(x.new) <- dim(x)
	x.new
}
