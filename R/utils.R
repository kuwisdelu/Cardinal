
## Match methods to their workhorse functions
.match.method <- function(method, which=-2) {
	tryCatch({
		base <- deparse(sys.call(which)[[1]])
		match.fun(paste(base, method, sep="."))
	}, error=function(e) match.fun(method))
}

## Evaluate a function after capturing unwanted ... arguments
.without <- function(exprs, ..., signature) {
	.local <- function() {
		eval(substitute(exprs, env=parent.frame()),
			envir=sys.frame(-2), enclos=sys.frame(-2))
	}
	if ( is.function(signature) ) {
		formals(.local) <- formals(signature)
	} else {
		formals(.local) <- signature
	}
	.local(...)
}
