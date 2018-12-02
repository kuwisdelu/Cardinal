
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
		# pixelData()
		if ( !is.null(pixelNames(object)) )
			.scat("pixelNames(%d): %s\n", pixelNames(object), prefix=t1)
		.scat("pixelData(%d): %s\n", names(pixelData(object)), prefix=t1)
		# metadata()
	    if ( length(metadata(object)) > 0L )
			.scat("metadata(%d): %s\n", names(metadata(object)), prefix=t1)
	}
)



## Filter pixels/features

setMethod("features", "ImagingExperiment",
	function(object, ..., .env = parent.frame(2)) {
		fdata <- featureData(object)
		conditions <- eval(substitute(alist(...)))
		e <- as.env(fdata, enclos=.env)
		if ( length(conditions) > 0 ) {
			features <- sapply(conditions, function(ci) {
				ci <- eval(ci, envir=e)
				if ( !is.logical(ci) ) 
					.stop("arguments must be logical vectors")
				rep_len(ci, nrow(fdata))
			})
			if ( nrow(fdata) == 1 )
				features <- t(features)
			if ( is.null(dim(features)) ) {
				features <- which(features)
			} else {
				features <- which(apply(features, 1, all))
			}	
		} else {
			features <- seq_len(nrow(fdata))
		}
		names(features) <- featureNames(object)[features]
		features
	})

setMethod("pixels", "ImagingExperiment",
	function(object, ..., .env = parent.frame(2)) {
		pdata <- pixelData(object)
		conditions <- eval(substitute(alist(...)))
		e <- as.env(pdata, enclos=.env)
		if ( length(conditions) > 0 ) {
			pixels <- sapply(conditions, function(ci) {
				ci <- eval(ci, envir=e)
				if ( !is.logical(ci) ) 
					.stop("argments must be logical vectors")
				rep_len(ci, nrow(pdata))
			})
			if ( nrow(pdata) == 1 )
				pixels <- t(pixels)
			if ( is.null(dim(pixels)) ) {
				pixels <- which(pixels)
			} else {
				pixels <- which(apply(pixels, 1, all))
			}
		} else {
			pixels <- seq_len(nrow(pixelData(object)))
		}
		names(pixels) <- pixelNames(object)[pixels]
		pixels
	})


## Subsetting

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

## Additional methods

setMethod("dim", "ImagingExperiment", function(x)
	c(Features=nrow(fData(x)), Pixels=nrow(pData(x))))

setMethod("dimnames", "ImagingExperiment", function(x)
	list(featureNames(x), pixelNames(x)))

setMethod("length", "ImagingExperiment", 
	function(x) as.integer(ncol(x)))

