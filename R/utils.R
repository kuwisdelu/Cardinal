
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

## UUID generation from https://gist.github.com/cbare/5979354
uuid <- function(uppercase=FALSE) { 
	hex_digits <- c(as.character(0:9), letters[1:6])
	hex_digits <- if (uppercase) toupper(hex_digits) else hex_digits
	y_digits <- hex_digits[9:12]
	paste(
		paste0(sample(hex_digits, 8, replace=TRUE), collapse=''),
		paste0(sample(hex_digits, 4, replace=TRUE), collapse=''),
		paste0('4', paste0(sample(hex_digits, 3, replace=TRUE), collapse=''), collapse=''),
		paste0(sample(y_digits,1), paste0(sample(hex_digits, 3, replace=TRUE), collapse=''), collapse=''),
		paste0(sample(hex_digits, 12, replace=TRUE), collapse=''),
		sep='-')
}
