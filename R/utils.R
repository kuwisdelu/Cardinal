
## Match methods to functions
.match.method <- function(method, which=-2) {
	tryCatch({
		base <- deparse(sys.call(which)[[1]])
		match.fun(paste(base, method, sep="."))
	}, error=function(e) match.fun(method))
}
