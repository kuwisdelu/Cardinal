
## Normalization

setMethod("normalize", "MSImagingArrays",
	function(object,
		method = c("tic", "rms", "reference"),
		scale = NA, ref = NULL, ...)
	{
		method <- match.arg(method)
		if ( is.na(scale) ) {
			if ( method == "reference" ) {
				if ( is.null(ref) )
					.Error("must provide 'ref' for method='reference'")
				scale <- 1
			} else {
				scale <- max(lengths(object))
			}
		}
		if ( method == "reference" ) {
			addProcessing(object,
				FUN=.normalize_FUN(method),
				id="intensity normalization", ...,
				scale=scale, ref=ref)
		} else {
			addProcessing(object,
				FUN=.normalize_FUN(method),
				id="intensity normalization", ...,
				scale=scale)
		}
	})

.normalize_FUN <- function(method)
{
	switch(method,
		tic = function(x, ...) {
			x$intensity <- matter::rescale_sum(x$intensity, ...)
			x
		},
		rms = function(x, ...) {
			x$intensity <- matter::rescale_rms(x$intensity, ...)
			x
		},
		reference = function(x, ...) {
			x$intensity <- matter::rescale_ref(x$intensity, ..., domain=x$mz)
			x
		})
}
