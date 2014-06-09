
## Match methods to their workhorse functions
match.method <- function(method) {
	if ( is.function(method) ) {
		deparse(substitute(method, env=parent.frame()))
	} else if ( is.character(method) ) {
		method[[1]]
	} else {
		"unknown"
	}
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
