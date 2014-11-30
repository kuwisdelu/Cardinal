
#### Methods for subsetting and retrieving results ####
## ---------------------------------------------------

setMethod("resultData", "ResultSet",
	function(object) object@resultData)

setReplaceMethod("resultData", "ResultSet",
	function(object, value) {
		object@resultData <- value
		if ( validObject(object) )
			object			
	})

setMethod("modelData", "ResultSet",
	function(object) object@modelData)

setReplaceMethod("modelData", "ResultSet",
	function(object, value) {
		object@modelData <- value
		if ( validObject(object) )
			object			
	})

setMethod("names", "ResultSet", function(x) {
	sapply(resultData(x), names)
})

setMethod("length", "ResultSet", function(x) {
	length(resultData(x))
})

setMethod("$", "ResultSet", function(x, name) {
	lapply(resultData(x), function(res) res[[name]])
})

setMethod("[[", "ResultSet", function(x, i, j, ...) {
	if ( missing(i) ) {
		x[..., drop=TRUE]
	} else if ( length(list(...)) > 0 ) {
		x[i, ..., drop=TRUE]
	} else {
		resultData(x)[[i]]
	}
})

setMethod("[", "ResultSet", function(x, i, j, ..., drop=TRUE) {
	dots <- list(...)
	if ( !all(names(dots) %in% varLabels(modelData(x))) )
		.stop("all arguments must appear in 'modelData'")
	if ( length(dots) > 0 ) {
		select <- sapply(seq_along(dots), function(i) {
			modelData(x)[[names(dots)[[i]]]] %in% dots[[i]]
		})
		if ( is.null(dim(select)) ) {
			select <- which(select)
		} else {
			select <- which(apply(select, 1, all))
		}
		if ( drop && length(select) == 1 ) {
			resultData(x)[[select]]
		} else {
			resultData(x)[select]
		}
	} else {
		resultData(x)
	}
})

setMethod("show", "ResultSet", function(object) {
	cat("An object of class '", class(object), "'\n\n", sep="")
	tryCatch(print(summary(object)),
		error=function(e) cat("No summary available\n"))
})

