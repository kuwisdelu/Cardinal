
## Match methods to their workhorse functions
match.method <- function(method, which=-2) {
	tryCatch({
		base <- deparse(sys.call(which)[[1]])
		match.fun(paste(base, method, sep="."))
	}, error=function(e) match.fun(method))
}

## Evaluate a function after capturing unwanted ... arguments
wrap <- function(exprs, ..., signature) {
	.local <- function() {
		eval(substitute(exprs, env=parent.frame()))
	}
	environment(.local) <- parent.frame()
	if ( is.function(signature) ) {
		formals(.local) <- formals(signature)
	} else {
		formals(.local) <- signature
	}
	.local(...)
}
