
## Group a DataFrame

setMethod("group_by", "DataFrame",
	function(.data, ..., add = FALSE, .drop = FALSE)
	{
		group_by(as(.data, "XDataFrame"), ..., add=add, .drop=.drop)
	})

setMethod("group_by", "XDataFrame",
	function(.data, ..., add = FALSE, .drop = FALSE)
	{
		e <- as.env(.data, enclos=parent.frame(2))
		expr <- eval(substitute(alist(...)))
		nm <- sapply(substitute(...()), deparse)
		if ( !is.null(names(expr)) ) {
			nz <- nzchar(names(expr))
			nm[nz] <- names(expr)[nz]
		}
		names(expr) <- nm
		groups <- lapply(expr, eval, envir=e)
		groups <- lapply(groups, as.factor)
		if ( .drop )
			groups <- lapply(groups, droplevels)
		if ( add ) {
			groups(.data) <- c(.data@groups, groups)
		} else {
			groups(.data) <- groups
		}
		.data
	})

setMethod("ungroup", "XDataFrame",
	function(x, ...)
	{
		groups(x) <- list()
		x
	})

