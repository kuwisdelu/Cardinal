
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

.normalize_FUN <- function(method,
	input = "intensity", output = input)
{
	FUN <- switch(method,
		tic = matter::rescale_sum,
		rms = matter::rescale_rms,
		reference = matter::rescale_ref)
	function(x, ...) {
		x[[output]] <- FUN(x[[input]], ...)
		x
	}
}

