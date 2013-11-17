
#### functions for smoothing ####

contrastFunction <- function(method) {
	if ( is.function(method) ) {
		return(method)
	} else if ( is.character(method) ) {
		if ( length(method) > 1 ) method <- method[[1]]
		switch(method,
			"none" = nullContrast,
			"suppress" = suppressContrast,
			"histeq" = histeqContrast,
			match.fun(method)
		)
	} else {
		stop("could not find matching function for ", substitute(method))
	}
}

nullContrast <- function(x, ...) identity(x)

suppressContrast <- function(x, top.percent=0.01, max.intensity, ...) {
	top.x <- quantile(x, 1 - top.percent, na.rm=TRUE)
	if ( missing(max.intensity) ) max.intensity <- top.x
	x[x > top.x] <- top.x
	max.intensity * x / max(x, na.rm=TRUE)
}

histeqContrast <- function(x, hist.breaks=100, min.intensity, max.intensity, ...) {
	x.drop <- as.numeric(x)
	breaks <- unique(quantile(x.drop, seq(from=0, to=1, length.out=hist.breaks),
		na.rm=TRUE))
	x.cut <- cut(as.numeric(x.drop), breaks, include.lowest=TRUE)
	x.new <- as.numeric(x.cut) / length(levels(x.cut))
	top.x <- tail(breaks[-length(breaks)], 1)
	if ( missing(min.intensity) ) min.intensity <- min(x.drop, na.rm=TRUE)
	if ( missing(max.intensity) ) max.intensity <- top.x
	x.new <- (max.intensity - min.intensity) * x.new
	x.new <- min.intensity + x.new
	dim(x.new) <- dim(x)
	x.new
}
