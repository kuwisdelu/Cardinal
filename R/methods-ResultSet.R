
#### Methods for subsetting and retrieving results ####
## ---------------------------------------------------

setMethod("resultData", "ResultSet",
	function(object) object@resultData)

setReplaceMethod("resultData", "ResultSet",
	function(object, value) {
		object@ResultData <- value
		if ( validObject(object) )
			object			
	})

setMethod("modelData", "SImageData",
	function(object) object@modelData)

setReplaceMethod("modelData", "SImageData",
	function(object, value) {
		object@modelData <- value
		if ( validObject(object) )
			object			
	})

setMethod("$", "ResultSet", function(x, name) {
	sapply(resultData(object), function(ob) ob[[name]])
})

setMethod("[[", "ResultSet", function(x, i, j, ...) {
	resultData(object)[[i]]
})

setMethod("[", "ResultSet", function(x, i, j, ...) {
	dots <- list(...)
	if ( !all(names(dots) %in% varLabels(x)) )
		stop("all arguments must appear in 'modelData'")
	if ( length(dots) > 0 ) {
		select <- sapply(seq_along(dots), function(i) {
			modelData(x)[[names(dots)[[i]]]] %in% dots[[i]]
		})
		if ( is.null(dim(select)) ) {
			select <- which(select)
		} else {
			select <- which(apply(select, 1, all))
		}
		resultData(x)[select]
	} else {
		resultData(x)
	}
})

