
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
	max.intensity * x / max(x, na.rm=TRUE)
}

contrast.enhance.histogram <- function(x, blocks=100, ...) {
	if ( all(is.na(x)) ) return(x)
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
