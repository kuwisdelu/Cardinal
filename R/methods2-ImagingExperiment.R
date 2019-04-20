
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
			features <- lapply(conditions, function(ci) {
				ci <- eval(ci, envir=e)
				if ( !is.logical(ci) && !all(is.wholenumber(ci)) )
					.stop("arguments must be integer or logical vectors")
				if ( is.logical(ci) ) {
					rep_len(ci, nrow(fdata))
				} else {
					ci
				}
			})
			if ( length(features) == 1 && is.logical(features[[1]]) ) {
				features <- which(features[[1]])
			} else {
				conds <- sapply(features, is.logical)
				if ( any(conds) ) {
					ll <- do.call("cbind", features[conds])
					i1 <- which(apply(ll, 1, all))
				} else {
					i1 <- seq_len(nrow(fdata))
				}
				if ( any(!conds) ) {
					i2 <- unlist(features[!conds])
				} else {
					i2 <- seq_len(nrow(fdata))
				}
				features <- sort(intersect(i1, i2))
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
			pixels <- lapply(conditions, function(ci) {
				ci <- eval(ci, envir=e)
				if ( !is.logical(ci) && !all(is.wholenumber(ci)) )
					.stop("arguments must be integer or logical vectors")
				if ( is.logical(ci) ) {
					rep_len(ci, nrow(pdata))
				} else {
					ci
				}
			})
			if ( length(pixels) == 1 && is.logical(pixels[[1]]) ) {
				pixels <- which(pixels[[1]])
			} else {
				conds <- sapply(pixels, is.logical)
				if ( any(conds) ) {
					ll <- do.call("cbind", pixels[conds])
					i1 <- which(apply(ll, 1, all))
				} else {
					i1 <- seq_len(nrow(pdata))
				}
				if ( any(!conds) ) {
					i2 <- unlist(pixels[!conds])
				} else {
					i2 <- seq_len(nrow(pdata))
				}
				pixels <- sort(intersect(i1, i2))
			}
		} else {
			pixels <- seq_len(nrow(pdata))
		}
		names(pixels) <- pixelNames(object)[pixels]
		pixels
	})


## Subsetting

setMethod("[[", c("ImagingExperiment", "ANY", "ANY"),
	function(x, i, j, ...) pixelData(x)[[i, ...]])

setReplaceMethod("[[", c("ImagingExperiment", "ANY", "ANY"),
	function(x, i, j, ..., value) {
		pixelData(x)[[i, ...]] <- value
		x
	})

.DollarNames.ImagingExperiment <- function(x, pattern = "")
	grep(pattern, names(pixelData(x)), value=TRUE)

setMethod("$", "ImagingExperiment",
	function(x, name) pixelData(x)[[name, exact=FALSE]])

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

