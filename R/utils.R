
## Convert names of data types to their size in number of bytes
sizeof <- function(datatype) {
	bytes <- c(`16-bit integer` = 2,
		`32-bit integer` = 4,
		`64-bit integer` = 8,
		`32-bit float` = 4,
		`64-bit float` = 8)
	bytes[as.character(datatype)]
}

## Make an annotation (factor) from regions-of-interest (logical)
make.annotation <- function(...) {
	regions <- data.frame(...)
	names <- names(regions)
	x <- as.character(rep(NA, nrow(regions)))
	for ( nm in names ) {
		x[regions[[nm]]] <- nm
	}
	if ( length(regions) == 1 ) {
		x[is.na(x)] <- paste("NOT", names[1])
	}
	as.factor(x)
}

## Match methods to their workhorse functions
match.method <- function(method, options) {
	if ( is.function(method) ) {
		tryCatch(deparse(substitute(method, env=parent.frame())),
			error = function(e) "unknown")
	} else if ( is.character(method) && missing(options) ) {
		method[1]
	} else if ( is.character(method) ) {
		matched <- pmatch(method[1], options)
		if ( is.na(matched) ) {
			method[1]
		} else {
			options[matched]
		}
	} else {
		"<unknown>"
	}
}

## Programmatic friendly version of base::subset
subdata <- function(data, subset, select, drop=FALSE) {
	subset <- subrows(data, subset=subset)
	data[subset,select,drop=drop]
}

## Programmatic friendly version of base::subset (only return row indices)
subrows <- function(data, subset) {
	subset <- sapply(seq_along(subset), function(i) {
		data[[names(subset)[[i]]]] %in% subset[[i]]
	})
	if ( is.null(dim(subset)) ) {
		which(subset)
	} else {
		which(apply(subset, 1, all))
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

## Saves the current .Random.seed
save.seed <- function() {
	.Cardinal$.Random.seed <- get(".Random.seed", envir=globalenv())
}

## Restores the saved .Random.seed
restore.seed <- function() {
	assign(".Random.seed", .Cardinal$.Random.seed, envir=globalenv())
}
