
#### ResultsList ####
## ------------------

.valid_ResultsList <- function(object)
{
	errors <- NULL
	cls_ok <- vapply(object@listData, is, logical(1L),
		class2=object@elementType)
	if ( !all(cls_ok) ) {
		errors <- c(errors, paste0("elements must be of type ",
			sQuote(object@elementType)))
	}
	if ( is.null(names(object)) )
		errors <- c(errors, paste0("elements must be named"))
	if ( is.null(errors) ) TRUE else errors
}

setValidity("ResultsList", .valid_ResultsList)

ResultsList <- function(..., featureData = NULL, pixelData = NULL)
{
	x <- SimpleList(...)
	if ( is.null(names(x)) )
		names(x) <- paste0("model", seq_along(x) - 1L)
	x <- new("ResultsList", x, elementType=class(x[[1L]])[1L])
	if ( !is.null(featureData) || !is.null(pixelData) )
	{
		if ( !(!is.null(featureData) && !is.null(pixelData)) )
			stop("both 'featureData' and 'pixelData' ",
				"must be specified if either is provided")
		new("SpatialResultsList", x,
			featureData=featureData, pixelData=pixelData)
	} else {
		x
	}
}

setMethod("show", "ResultsList",
	function(object) {
		callNextMethod()
		cat("elementType:", object@elementType, "\n")
		# resultNames()
		cat(sprintf("resultNames(%d): %s\n", length(resultNames(object)),
			.paste_head_tail(resultNames(object))))
		# modelData()
		if ( length(mcols(object)) > 0L )
		{
			cat(sprintf("modelData(%d): %s\n", length(mcols(object)),
				.paste_head_tail(names(mcols(object)))))
		}
	})

## Slot getters and setters

# modelData

setMethod("modelData", "ResultsList",
	function(object, ...) mcols(object))
setReplaceMethod("modelData", "ResultsList",
	function(object, ..., value) {
		mcols(object) <- value
		if ( validObject(object) )
			object
	})

# resultData

setMethod("resultData", "ResultsList",
	function(object, i, ...)
	{
		lapply(object, function(x) x[[i, exact=FALSE]])
	})
setReplaceMethod("resultData", "ResultsList",
	function(object, i, ..., value)
	{
		value <- as.list(value)
		if ( length(value) != length(object) )
			stop("length of replacement [", length(value), "] ",
				"does not match length of object [", length(object), "]")
		for ( j in seq_along(object) )
			object[[j]][[i]] <- value[[j]]
		if ( validObject(object) )
			object
	})

# resultNames

setMethod("resultNames", "ResultsList",
	function(object) unique(unlist(lapply(object, names))))
setReplaceMethod("resultNames", "ResultsList",
	function(object, value) {
		stop("can't replace resultNames")
	})

#### SpatialResultsList ####
## -------------------------

setMethod("show", "SpatialResultsList",
	function(object) {
		callNextMethod()
		# featureData()
		cat(sprintf("featureData(%d): %s\n", length(featureData(object)),
			.paste_head_tail(names(featureData(object)))))
		# pixelData()
		cat(sprintf("pixelData(%d): %s\n", length(pixelData(object)),
			.paste_head_tail(names(pixelData(object)))))
		# runNames()
		cat(sprintf("runNames(%d): %s\n", length(runNames(object)),
			.paste_head_tail(runNames(object))))
		# coord()
		if ( length(object) > 0L )
		{
			lims <- vapply(coord(object), range, numeric(2L))
			lims <- paste0(coordNames(object), " = ", lims[1L,], "...", lims[2L,])
			cat(sprintf("coord(%d): %s\n", length(coordNames(object)),
				.paste_head_tail(lims)))
		}
	})

## Slot getters and setters

# featureData

setMethod("featureData", "SpatialResultsList",
	function(object) object@featureData)
setReplaceMethod("featureData", "SpatialResultsList",
	function(object, value) {
		object@featureData <- value
		if ( validObject(object) )
			object
	})

setMethod("fData", "SpatialResultsList",
	function(object) featureData(object))
setReplaceMethod("fData", "SpatialResultsList",
	function(object, value) {
		featureData(object) <- value
		object
	})

setMethod("featureNames", "SpatialResultsList",
	function(object) rownames(featureData(object)))
setReplaceMethod("featureNames", "SpatialResultsList",
	function(object, value) {
		rownames(featureData(object)) <- value
			object
	})

# pixelData

setMethod("pixelData", "SpatialResultsList",
	function(object) object@pixelData)
setReplaceMethod("pixelData", "SpatialResultsList",
	function(object, value) {
		object@pixelData <- value
		if ( validObject(object) )
			object
	})

setMethod("pData", "SpatialResultsList",
	function(object) pixelData(object))
setReplaceMethod("pData", "SpatialResultsList",
	function(object, value) {
		pixelData(object) <- value
		object
	})

setMethod("pixelNames", "SpatialResultsList",
	function(object) rownames(pixelData(object)))
setReplaceMethod("pixelNames", "SpatialResultsList",
	function(object, value) {
		rownames(pixelData(object)) <- value
		object
	})

# Coord/Run access

setMethod("coord", "SpatialResultsList",
	function(object, ...) coord(pixelData(object)))
setReplaceMethod("coord", "SpatialResultsList",
	function(object, ..., value) {
		coord(pixelData(object)) <- value
		object
	})

setMethod("coordNames", "SpatialResultsList",
	function(object) coordNames(pixelData(object)))
setReplaceMethod("coordNames", "SpatialResultsList",
	function(object, value) {
		coordNames(pixelData(object)) <- value
		object
	})

setMethod("run", "SpatialResultsList",
	function(object, ...) run(pixelData(object)))
setReplaceMethod("run", "SpatialResultsList",
	function(object, ..., value) {
		run(pixelData(object)) <- value
		object
	})

setMethod("runNames", "SpatialResultsList",
	function(object) runNames(pixelData(object)))
setReplaceMethod("runNames", "SpatialResultsList",
	function(object, value) {
		runNames(pixelData(object)) <- value
		object
	})

setMethod("nrun", "SpatialResultsList",
	function(x) nrun(pixelData(x)))

