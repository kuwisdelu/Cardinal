
#### Methods for ImagingExperiment ####
## ------------------------------------

## imageData methods

setMethod("imageData", "ImagingExperiment", 
	function(y) y@imageData)

setReplaceMethod("imageData", "ImagingExperiment",
	function(y, value) {
		y@imageData <- value
		if ( validObject(y) )
			y
	})

setMethod("iData", c("ImagingExperiment", "missing"), 
	function(x, i, ...) x@imageData[[1]])

setMethod("iData", c("ImagingExperiment", "ANY"), 
	function(x, i, ...) x@imageData[[i]])

setReplaceMethod("iData", c("ImagingExperiment", "missing"),
	function(x, i, ..., value) {
		x@imageData[[1]] <- value
		if ( validObject(x) )
			x
	})

setReplaceMethod("iData", c("ImagingExperiment", "ANY"),
	function(x, i, ..., value) {
		x@imageData[[i]] <- value
		if ( validObject(x) )
			x
	})

## featureData methods

setMethod("featureData", "ImagingExperiment", 
	function(object) object@featureData)

setReplaceMethod("featureData", "ImagingExperiment",
	function(object, value) {
		object@featureData <- value
		if ( validObject(object) )
			object
	})

setMethod("fData", "ImagingExperiment",
	function(object) featureData(object))

setReplaceMethod("fData", "ImagingExperiment",
	function(object, value) {
		featureData(object) <- value
		object
	})

setMethod("featureNames", "ImagingExperiment",
	function(object) rownames(featureData(object)))

setReplaceMethod("featureNames", "ImagingExperiment",
	function(object, value) {
		rownames(featureData(object)) <- value
		object
	})

## pixelData methods

setMethod("pixelData", "ImagingExperiment", 
	function(object) object@elementMetadata)

setReplaceMethod("pixelData", "ImagingExperiment",
	function(object, value) {
		object@elementMetadata <- value
		if ( validObject(object) )
			object
	})

setMethod("pData", "ImagingExperiment",
	function(object) pixelData(object))

setReplaceMethod("pData", "ImagingExperiment",
	function(object, value) {
		pixelData(object) <- value
		object
	})

setMethod("pixelNames", "ImagingExperiment",
	function(object) rownames(pixelData(object)))

setReplaceMethod("pixelNames", "ImagingExperiment",
	function(object, value) {
		rownames(pixelData(object)) <- value
		object
	})

setMethod("coord", "ImagingExperiment",
	function(object) coord(pixelData(object)))

setReplaceMethod("coord", "ImagingExperiment",
	function(object, value) {
		coord(pixelData(object)) <- value
		object
	})

setMethod("coordnames", "ImagingExperiment",
	function(x) coordnames(pixelData(x)))

setReplaceMethod("coordnames", "ImagingExperiment",
	function(x, value) {
		coordnames(pixelData(x)) <- value
		x
	})

setMethod("coordLabels", "ImagingExperiment",
	function(object) coordnames(object))

setReplaceMethod("coordLabels", "ImagingExperiment",
	function(object, value) {
		coordnames(object) <- value
		object
	})

setMethod("gridded", "ImagingExperiment",
	function(obj) gridded(pixelData(obj)))

setReplaceMethod("gridded", "ImagingExperiment",
	function(obj, value) {
		gridded(pixelData(obj)) <- value
		obj
	})

setMethod("resolution", "ImagingExperiment",
	function(object) resolution(pixelData(object)))

setReplaceMethod("resolution", "ImagingExperiment",
	function(object, value) {
		resolution(pixelData(object)) <- value
		object
	})

setMethod("dims", "ImagingExperiment",
	function(object) dims(pixelData(object)))

## Additional methods

setMethod("[[", c("ImagingExperiment", "ANY", "missing"),
	function(x, i, j, ...) pixelData(x)[[i, ...]])

setReplaceMethod("[[", c("ImagingExperiment", "ANY", "missing"),
	function(x, i, j, ..., value) {
		pixelData(x)[[i, ...]] <- value
		x
	})

.DollarNames.ImagingExperiment <- function(x, pattern = "")
	grep(pattern, names(pixelData(x)), value=TRUE)

setMethod("$", "ImagingExperiment",
	function(x, name) pixelData(x)[[name]])

setReplaceMethod("$", "ImagingExperiment",
	function(x, name, value) {
		pixelData(x)[[name]] <- value
		x
	})

setMethod("dim", "ImagingExperiment", function(x)
	c(Features=nrow(fData(x)), Pixels=nrow(pData(x))))

setMethod("dimnames", "ImagingExperiment", function(x)
	list(featureNames(x), pixelNames(x)))


