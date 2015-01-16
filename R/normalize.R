
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

normalize.image.linear <- function(x, min=0, max=100, ...) {
	if ( all(is.na(x)) ) return(x)
	oldmin <- min(x, na.rm=TRUE)
	oldmax <- max(x, na.rm=TRUE)
	((x - oldmin) * (max - min) / (oldmax - oldmin)) + min
}
