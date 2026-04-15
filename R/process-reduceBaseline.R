
## Baseline removal

setMethod("reduceBaseline", "MSImagingArrays",
	function(object,
		method = c("locmin", "hull", "snip", "median"), ...)
	{
		method <- match.arg(method)
		addProcessing(object,
			FUN=.reduceBaseline_FUN(method),
			id="baseline reduction", ...)
	})

.reduceBaseline_FUN <- function(method)
{
	switch(method,
		locmin = function(x, ...) {
			if ( length(x$intensity) <= 3L )
				return(x)
			b <- matter::estbase_loc(x$intensity, ...)
			x$intensity <- pmax(0, x$intensity - b)
			x
		},
		hull = function(x, ...) {
			if ( length(x$intensity) <= 3L )
				return(x)
			b <- matter::estbase_hull(x$intensity, ...)
			x$intensity <- pmax(0, x$intensity - b)
			x
		},
		snip = function(x, ...) {
			if ( length(x$intensity) <= 3L )
				return(x)
			b <- matter::estbase_snip(x$intensity, ...)
			x$intensity <- pmax(0, x$intensity - b)
			x
		},
		median = function(x, ...) {
			if ( length(x$intensity) <= 3L )
				return(x)
			b <- matter::estbase_med(x$intensity, ...)
			x$intensity <- pmax(0, x$intensity - b)
			x
		})
}
