
setMethod("initialize", "RImageData",
	function(.Object,
			varMetadata = list(),
			...) {
		.Object <- callNextMethod(.Object, ...)
		.Object@varMetadata <- varMetadata
		names <- ls(.Object@data)
		varMetadata <- lapply(names, function(nm) {
			if ( !is.null(colnames(.Object@data[[nm]])) ) {
				rows <- as.character(colnames(.Object@data[[nm]]))
			} else {
				rows <- as.character(seq_len(ncol(.Object@data[[nm]])))
			}
			data.frame(row.names=rows)
		})
		names(varMetadata) <- names
		missingMeta <- setdiff(names(varMetadata), names(.Object@varMetadata))
		.Object@varMetadata[missingMeta] <- varMetadata[missingMeta]
		if ( validObject(.Object) )
			.Object
	})

RImageData <- function(..., data = new.env(parent=emptyenv()),
	storageMode = c("immutableEnvironment",
		"lockedEnvironment", "environment"),
	varMetadata = list())
{
	storageMode <- match.arg(storageMode)
	dots <- match.call(expand.dots=FALSE)$...
	names <- names(dots)
	if ( any(is.null(names)) && length(dots) > 0 )
		stop("all elements must be named")
	.RImageData(data=data,
		storageMode=storageMode,
		varMetadata=varMetadata,
		...)
}

setValidity("RImageData", function(object) {
	msg <- validMsg(NULL, NULL)
	names <- ls(object@data)
	if ( !all(sapply(names, function(nm) length(dim(object@data[[nm]]))) == 2) )
		msg <- validMsg(msg, "all data elements must have 'dims' of length 2")
	nrows <- sapply(names, function(nm) nrow(object@data[[nm]]))
	if ( !all(sapply(nrows, function(nr) nr == nrows[[1]])) )
		msg <- validMsg(msg, "all elements must have an equal number of rows")
	if ( !all(names == ls(object@varMetadata)) )
		msg <- validMsg(msg, "data and varMetadata must have identical names")
	if ( !all(sapply(object@varMetadata, is.data.frame)) )
		msg <- validMsg(msg, "all elements of varMetadata must be a data.frame")
	if ( !all(sapply(names, function(nm) nrow(object@varMetadata[[nm]]) == ncol(object@data[[nm]]))) )
		msg <- validMsg(msg, paste("elements of varMetadata have number of rows equal
			to the number of columns in the corresponding element of data"))
	if (is.null(msg)) TRUE else msg
})

setMethod("varMetadata", "RImageData", function(object) object@varMetadata)

setReplaceMethod("varMetadata", "RImageData",
	function(object, value) {
		object@varMetadata <- value
		if ( validObject(object) )
			object
	})

setMethod("$", "RImageData", function(x, name) x[[name]])

setReplaceMethod("$", "RImageData",
	function(x, name, value) {
		x[[name]] <- value
		x
	})

setReplaceMethod("[[", signature(x="RImageData", i="character", j="missing"),
	function(x, i, j, ..., value) {
		dots <- list(...)
		if ( length(dots) > 0 ) {
			x@varMetadata[[i]] <- as.data.frame(dots)
		} else {
			if ( !is.null(colnames(value)) ) {
				rows <- as.character(colnames(value))
			} else {
				rows <- as.character(seq_len(ncol(value)))
			}
			x@varMetadata[[i]] <- data.frame(row.names=rows)
		}
		x <- callNextMethod(x, i=i, value=value)
		if ( validObject(x) )
			x
	})
