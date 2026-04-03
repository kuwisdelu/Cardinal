
#### Internal utility functions ####
## ---------------------------------

.bpiterate <- function(ITER, FUN, ...,
	REDUCE, init, reduce.in.order = FALSE,
	BPPARAM = NULL, BPOPTIONS = list())
{
	if ( is.null(BPPARAM) ) {
		if ( missing(REDUCE) ) {
			if ( !missing(init) )
				stop("'REDUCE' must be provided when 'init' is used")
			REDUCE <- function(result, item) c(result, list(item))
			init <- NULL
		}
		ans <- init
		while ( !is.null(X <- ITER()) ) {
			ans <- REDUCE(ans, FUN(X, ...))
		}
	} else {
		ans <- bpiterate(ITER, FUN, ...,
			REDUCE=REDUCE,
			init=init,
			reduce.in.order=reduce.in.order,
			BPPARAM=BPPARAM,
			BPOPTIONS=BPOPTIONS)
	}
	ans
}
