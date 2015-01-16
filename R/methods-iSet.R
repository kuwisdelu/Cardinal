
setMethod("initialize", "iSet",
	function(.Object,
			imageData = ImageData(),
			pixelData = annotatedDataFrameFrom(imageData),
			featureData = AnnotatedDataFrame(),
			protocolData = AnnotatedDataFrame(),
			experimentData = new("MIAPE-Imaging"),
			...) {
		if ( !missing(pixelData) )
			dimLabels(pixelData) <- c("pixelNames", "pixelColumns")
		if ( !missing(featureData) )
			dimLabels(featureData) <- c("featureNames", "featureColumns")
		if ( !missing(protocolData) ) {
			dimLabels(protocolData) <- c("sampleNames", "sampleColumns")
			if ( !missing(pixelData) )
				sampleNames(protocolData) <- sampleNames(pixelData)
		}
		callNextMethod(.Object,
			imageData=imageData,
			pixelData=pixelData,
			featureData=featureData,
			protocolData=protocolData,
			experimentData=experimentData,
			...)
	})

setValidity("iSet", function(object) {
	msg <- validMsg(NULL, NULL)
	dims <- dims(object)
	if ( ncol(dims) > 0 ) {
		if ( (nrow(dims) - 1) != ncol(coord(object@pixelData)) ) # assume dims[1,] are features
			msg <- validMsg(msg, "number of spatial dimensions differ between imageData and pixelData")
		if ( !all(rownames(dims)[-1] == names(coord(object@pixelData))) )
			msg <- validMsg(msg, "dimension names differ between imageData and pixelData")
		if ( !all(sampleNames(object@pixelData) == sampleNames(object@protocolData)) )
			msg <- validMsg(msg, "sample names differ between pixelData and protocolData")
	}
	if (is.null(msg)) TRUE else msg
})

#### imageData methods ####
## ------------------------

setMethod("imageData", "iSet", function(object) object@imageData)

setReplaceMethod("imageData", "iSet",
	function(object, value) {
		object@imageData <- value
		object
	})

#### featureData methods ####
## --------------------------

setMethod("featureData", "iSet", function(object) object@featureData)

setReplaceMethod("featureData", "iSet",
	function(object, value) {
		object@featureData <- value
		object
	})

setMethod("fData", "iSet", function(object) pData(featureData(object)))

setReplaceMethod("fData", "iSet",
	function(object, value) {
		pData(featureData(object)) <- value
		object
	})

setMethod("fvarMetadata", "iSet", function(object) varMetadata(featureData(object)))

setReplaceMethod("fvarMetadata", "iSet",
	function(object, value) {
		varMetadata(featureData(object)) <- value
		object
	})

setMethod("fvarLabels", "iSet", function(object) varLabels(featureData(object)))

setReplaceMethod("fvarLabels", "iSet",
	function(object, value) {
		varLabels(featureData(object)) <- value
		object
	})

setMethod("featureNames", "iSet", function(object) featureNames(featureData(object)))

setReplaceMethod("featureNames", "iSet",
	function(object, value) {
		featureNames(featureData(object)) <- value
		object
	})

#### pixelData methods ####
## ------------------------

setMethod("pixelData", "iSet", function(object) object@pixelData)

setReplaceMethod("pixelData", "iSet",
	function(object, value) {
		object@pixelData <- value
		object
	})

setMethod("pData", "iSet", function(object) pData(pixelData(object)))

setReplaceMethod("pData", "iSet",
	function(object, value) {
		pData(pixelData(object)) <- value
		object
	})

setMethod("varMetadata", "iSet", function(object) varMetadata(pixelData(object)))

setReplaceMethod("varMetadata", "iSet",
	function(object, value) {
		varMetadata(pixelData(object)) <- value
		object
	})

setMethod("varLabels", "iSet", function(object) varLabels(pixelData(object)))

setReplaceMethod("varLabels", "iSet",
	function(object, value) {
		varLabels(pixelData(object)) <- value
		object
	})

setMethod("sampleNames", "iSet", function(object) sampleNames(pixelData(object)))

setReplaceMethod("sampleNames", "iSet",
	function(object, value) {
		sampleNames(object@pixelData) <- value
		sampleNames(object@protocolData) <- value
		object
	})

setMethod("pixelNames", "iSet", function(object) pixelNames(pixelData(object)))

setReplaceMethod("pixelNames", "iSet",
	function(object, value) {
		pixelNames(pixelData(object)) <- value
		object
	})

setMethod("coordLabels", "iSet", function(object) coordLabels(pixelData(object)))

setReplaceMethod("coordLabels", "iSet",
	function(object, value) {
		coordLabels(pixelData(object)) <- value
		object
	})

setMethod("coord", "iSet", function(object) coord(pixelData(object)))

setReplaceMethod("coord", "iSet",
	function(object, value) {
		coord(pixelData(object)) <- value
		object
	})

#### Pixel and feature lookup ####
## -------------------------------

setMethod("features", "iSet",
	function(object, ...) {
		dots <- list(...)
		if ( !all(names(dots) %in% fvarLabels(object)) )
			.stop("all arguments must appear as variables in 'featureData'")
		if ( length(dots) > 0 ) {
			features <- sapply(seq_along(dots), function(i) {
				fData(object)[[names(dots)[[i]]]] %in% dots[[i]]
			})
			if ( nrow(fData(object)) == 1 )
				features <- t(features)
			if ( is.null(dim(features)) ) {
				features <- which(features)
			} else {
				features <- which(apply(features, 1, all))
			}	
		} else {
			features <- seq_len(nrow(fData(object)))
		}
		names(features) <- featureNames(object)[features]
		features
	})

setMethod("pixels", "iSet",
	function(object, ...) {
		dots <- list(...)
		if ( !all(names(dots) %in% varLabels(object)) )
			.stop("all arguments must appear as variables in 'pixelData'")
		if ( length(dots) > 0 ) {
			pixels <- sapply(seq_along(dots), function(i) {
				pData(object)[[names(dots)[[i]]]] %in% dots[[i]]
			})
			if ( nrow(pData(object)) == 1 )
				pixels <- t(pixels)
			if ( is.null(dim(pixels)) ) {
				pixels <- which(pixels)
			} else {
				pixels <- which(apply(pixels, 1, all))
			}
		} else {
			pixels <- seq_len(nrow(pData(object)))
		}
		names(pixels) <- pixelNames(object)[pixels]
		pixels
	})

#### Other slots ####
## ------------------

setMethod("protocolData", "iSet", function(object) object@protocolData)

setReplaceMethod("protocolData", "iSet",
	function(object, value) {
		object@protocolData <- value
		object
	})

setMethod("experimentData", "iSet", function(object) object@experimentData)

setReplaceMethod("experimentData", "iSet",
	function(object, value) {
		object@experimentData <- value
		object
	})

#### Standard generic methods ####
## -------------------------------

# Adapted from combine(eSet, eSet) from Biobase
setMethod("combine", signature = c(x = "iSet", y = "iSet"),
	function(x, y, ...) {
		if (class(x) != class(y))
			.stop("objects must be the same class, but are '",
				class(x), "', '", class(y), "'")
		if ( !isCurrent(x)[["iSet"]] )
			x <- updateObject(x)
		x@imageData <- combine(x@imageData, y@imageData)
		x@pixelData <- combine(x@pixelData, y@pixelData)
		x@featureData <- combine(x@featureData, y@featureData)
		x@experimentData <- combine(x@experimentData, y@experimentData)
		x@protocolData <- combine(x@protocolData, y@protocolData)
		x
	})

setMethod("dim", "iSet", function(x)
	c(Features=nrow(fData(x)), Pixels=nrow(pData(x))))

setMethod("dims", "iSet", function(object) dims(imageData(object)))

setMethod("storageMode", "iSet", function(object) storageMode(imageData(object)))

setReplaceMethod("storageMode", "iSet",
	function(object, value) {
		storageMode(imageData(object)) <- value
		object
	})

setMethod("$", "iSet", function(x, name) pixelData(x)[[name]])

setReplaceMethod("$", "iSet",
	function(x, name, value) {
		pixelData(x)[[name]] <- value
		x
	})

setMethod("[[", "iSet", function(x, i, j, ...) pixelData(x)[[i]])

setReplaceMethod("[[", "iSet",
	function(x, i, j, ..., value) {
		pixelData(x)[[i, ...]] <- value
		x
	})

