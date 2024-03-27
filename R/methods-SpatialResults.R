
#### SpatialResults ####
## ---------------------

SpatialResults <- function(model, data,
	featureData. = featureData(data), pixelData. = pixelData(data))
{
	new("SpatialResults", model=model,
		featureData=featureData., pixelData=pixelData.)
}

setMethod("show", "SpatialResults",
	function(object) {
		# length
		cat(class(object), "with", length(object), "components\n")
		# names()
		cat(sprintf("names(%d): %s\n", length(names(object)),
			.paste_head_tail(names(object))))
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

## Basic getters and setters

setMethod("length", "SpatialResults", function(x) length(x@model))

setMethod("names", "SpatialResults",
	function(x) names(x@model))

setMethod("[[", "SpatialResults",
	function(x, i, j, ...) x@model[[i, ...]])

setMethod("$", "SpatialResults",
	function(x, name) x@model[[name, exact=FALSE]])

# allow tab completion in console
.DollarNames.SpatialResults <- function(x, pattern = "") {
	grep(pattern, names(x), value=TRUE)
}

## Slot getters and setters

# modelData

setMethod("modelData", "SpatialResults",
	function(object, ...) object@model)
setReplaceMethod("modelData", "SpatialResults",
	function(object, ..., value) {
		object@model <- value
		if ( validObject(object) )
			object
	})

# featureData

setMethod("featureData", "SpatialResults",
	function(object) object@featureData)
setReplaceMethod("featureData", "SpatialResults",
	function(object, value) {
		object@featureData <- value
		if ( validObject(object) )
			object
	})

setMethod("fData", "SpatialResults",
	function(object) featureData(object))
setReplaceMethod("fData", "SpatialResults",
	function(object, value) {
		featureData(object) <- value
		object
	})

setMethod("featureNames", "SpatialResults",
	function(object) rownames(featureData(object)))
setReplaceMethod("featureNames", "SpatialResults",
	function(object, value) {
		rownames(featureData(object)) <- value
			object
	})

# pixelData

setMethod("pixelData", "SpatialResults",
	function(object) object@pixelData)
setReplaceMethod("pixelData", "SpatialResults",
	function(object, value) {
		object@pixelData <- value
		if ( validObject(object) )
			object
	})

setMethod("pData", "SpatialResults",
	function(object) pixelData(object))
setReplaceMethod("pData", "SpatialResults",
	function(object, value) {
		pixelData(object) <- value
		object
	})

setMethod("pixelNames", "SpatialResults",
	function(object) rownames(pixelData(object)))
setReplaceMethod("pixelNames", "SpatialResults",
	function(object, value) {
		rownames(pixelData(object)) <- value
		object
	})

# Coord/Run access

setMethod("coord", "SpatialResults",
	function(object, ...) coord(pixelData(object)))
setReplaceMethod("coord", "SpatialResults",
	function(object, ..., value) {
		coord(pixelData(object)) <- value
		object
	})

setMethod("coordNames", "SpatialResults",
	function(object) coordNames(pixelData(object)))
setReplaceMethod("coordNames", "SpatialResults",
	function(object, value) {
		coordNames(pixelData(object)) <- value
		object
	})

setMethod("run", "SpatialResults",
	function(object, ...) run(pixelData(object)))
setReplaceMethod("run", "SpatialResults",
	function(object, ..., value) {
		run(pixelData(object)) <- value
		object
	})

setMethod("runNames", "SpatialResults",
	function(object) runNames(pixelData(object)))
setReplaceMethod("runNames", "SpatialResults",
	function(object, value) {
		runNames(pixelData(object)) <- value
		object
	})

setMethod("nrun", "SpatialResults",
	function(x) nrun(pixelData(x)))


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
	if ( is.null(errors) ) TRUE else errors
}

setValidity("ResultsList", .valid_ResultsList)

ResultsList <- function(..., mcols = NULL)
{
	if ( ...length() == 1L && is(..1, "list_OR_List") ) {
		if ( is(..1, "List") ) {
			x <- as(..1, "SimpleList")
		} else {
			x <- SimpleList(..1)
		}
	} else {
		x <- SimpleList(...)
	}
	new("ResultsList", x, elementMetadata=mcols,
		elementType=class(x[[1L]])[1L])
}

setMethod("show", "ResultsList",
	function(object) {
		callNextMethod()
		cat("model:", object@elementType, "\n")
		if ( !is.null(mcols(object)) )
			print(as.data.frame(mcols(object)))
	})
