
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
	function(x, i, ...) x@imageData[[i, exact=FALSE]])

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

setMethod("fData", "ImagingExperiment",
	function(object) object@featureData)

setReplaceMethod("fData", "ImagingExperiment",
	function(object, value) {
		object@featureData <- value
		if ( validObject(object) )
			object
	})

setMethod("featureData", "ImagingExperiment", 
	function(object) object@featureData)

setReplaceMethod("featureData", "ImagingExperiment",
	function(object, value) {
		object@featureData <- value
		if ( validObject(object) )
			object
	})

setMethod("featureNames", "ImagingExperiment",
	function(object) rownames(featureData(object)))

setReplaceMethod("featureNames", "ImagingExperiment",
	function(object, value) {
		rownames(featureData(object)) <- value
		object
	})

## elementMetadata methods


setMethod("pData", "ImagingExperiment",
	function(object) object@elementMetadata)

setReplaceMethod("pData", "ImagingExperiment",
	function(object, value) {
		object@elementMetadata <- value
		if ( validObject(object) )
			object
	})

setMethod("phenoData", "ImagingExperiment", 
	function(object) object@elementMetadata)

setReplaceMethod("phenoData", "ImagingExperiment",
	function(object, value) {
		object@elementMetadata <- value
		if ( validObject(object) )
			object
	})

setMethod("sampleNames", "ImagingExperiment",
	function(object) rownames(elementMetadata(object)))

setReplaceMethod("sampleNames", "ImagingExperiment",
	function(object, value) {
		rownames(elementMetadata(object)) <- value
		object
	})

# redefine in sub-classes if columns = pixels
setMethod("pixelData", "ImagingExperiment", function(object) NULL)

setReplaceMethod("pixelData", "ImagingExperiment",
	function(object, value) object)

# redefine in sub-classes if columns = pixels
setMethod("pixelNames", "ImagingExperiment", function(object) NULL)

setReplaceMethod("pixelNames", "ImagingExperiment",
	function(object, value) object)


## show

setMethod("show", "ImagingExperiment",
	function(object) {
		cat("An object of class '", class(object), "'\n", sep="")
		cat("  <", nrow(object), " feature, ", ncol(object), " pixel> ",
			"imaging dataset", "\n", sep="")
		t1 <- "    "
		# imageData()
		imageDataNames <- names(imageData(object))
		if ( is.null(imageDataNames) )
			imageDataNames <- character(length(imageData(object)))
		.scat("imageData(%d): %s\n", imageDataNames, prefix=t1)
		# featureData()
		if ( !is.null(featureNames(object)) )
			.scat("featureNames(%d): %s\n", featureNames(object), prefix=t1)
		.scat("featureData(%d): %s\n", names(featureData(object)), prefix=t1)
		# check if columns = pixels
		if ( is.null(pixelData(object)) ) {
			# phenoData()
			if ( !is.null(sampleNames(object)) )
				.scat("sampleNames(%d): %s\n", sampleNames(object), prefix=t1)
			.scat("phenoData(%d): %s\n", names(phenoData(object)), prefix=t1)
		} else {
			# pixelData()
			if ( !is.null(pixelNames(object)) )
				.scat("pixelNames(%d): %s\n", pixelNames(object), prefix=t1)
			.scat("pixelData(%d): %s\n", names(pixelData(object)), prefix=t1)
		}
		# metadata()
	    if ( length(metadata(object)) > 0L )
			.scat("metadata(%d): %s\n", names(metadata(object)), prefix=t1)
	}
)


## Subsetting

setMethod("[[", c("ImagingExperiment", "ANY", "ANY"),
	function(x, i, j, ...) elementMetadata(x)[[i, ...]])

setReplaceMethod("[[", c("ImagingExperiment", "ANY", "ANY"),
	function(x, i, j, ..., value) {
		elementMetadata(x)[[i, ...]] <- value
		x
	})

.DollarNames.ImagingExperiment <- function(x, pattern = "")
	grep(pattern, names(elementMetadata(x)), value=TRUE)

setMethod("$", "ImagingExperiment",
	function(x, name) elementMetadata(x)[[name, exact=FALSE]])

setReplaceMethod("$", "ImagingExperiment",
	function(x, name, value) {
		elementMetadata(x)[[name]] <- value
		x
	})

## Additional methods

setMethod("names", "ImagingExperiment",
	function(x) rownames(x@elementMetadata))

setReplaceMethod("names", "ImagingExperiment",
	function(x, value) {
		rownames(x@elementMetadata) <- value
		x
	})

setMethod("dim", "ImagingExperiment",
	function(x) c(nrow(fData(x)), nrow(pData(x))))

setMethod("dimnames", "ImagingExperiment",
	function(x) list(featureNames(x), names(x)))

setMethod("length", "ImagingExperiment",
	function(x) unname(ncol(x)))


