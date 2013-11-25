
#### functions for image interpolation ####

interpolateFunction <- function(method) {
	if ( is.function(method) ) {
		return(method)
	} else if ( is.character(method) ) {
		if ( length(method) > 1 ) method <- method[[1]]
		switch(method,
			"none" = interpolateNull,
			"bilinear" = interpolateBilinear,
			match.fun(method)
		)
	} else {
		stop("could not find matching function for ", substitute(method))
	}
}

interpolateNull <- function(x, ...) identity(x)

interpolateBilinear <- function(x, xres=2, size=xres*dim(x), ...) {
	obj <- list(x=1:dim(x)[[1]], y=1:dim(x)[[2]], z=x)
	grid.list <- list(x=seq(1, dim(x)[[1]], length.out=size[[1]]),
		y=seq(1, dim(x)[[2]], length.out=size[[2]]))
	interp.surface.grid(obj, grid.list)$z
}
