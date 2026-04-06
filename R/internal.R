
#### Internal utility functions ####
## ---------------------------------

# BiocParallel::bpiterate with fallback when BPPARAM is NULL
.bpiterate <- function(ITER, FUN, ...,
	REDUCE, init, reduce.in.order = FALSE,
	BPREDO = list(), BPPARAM = NULL, BPOPTIONS = list())
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
			BPREDO=BPREDO,
			BPPARAM=BPPARAM,
			BPOPTIONS=BPOPTIONS)
	}
	ans
}

# take list of parallel lists and return list of tuples
.zipup <- function(lists)
{
	lists <- as.list(lists)
	lists <- lapply(lists, as.list)
	if ( length(lists) > 0L ) {
		tuples <- vector("list", length=length(lists[[1L]]))
		for ( i in seq_along(tuples) ) {
			xi <- lapply(seq_along(lists), function(j) lists[[j]][[i]])
			xi <- setNames(xi, names(lists))
			tuples[[i]] <- xi
		}
	} else {
		tuples <- list()
	}
	tuples
}

# take list of tuples and return list of parallel lists
.zipdown <- function(tuples)
{
	tuples <- as.list(tuples)
	tuples <- lapply(tuples, as.list)
	if ( length(tuples) > 0L ) {
		lists <- vector("list", length=length(tuples[[1L]]))
		lists <- setNames(lists, names(tuples[[1L]]))
		for ( j in seq_along(lists) ) {
			xj <- lapply(seq_along(tuples), function(i) tuples[[i]][[j]])
			lists[[j]] <- xj
		}
	} else {
		lists <- list()
	}
	lists
}
