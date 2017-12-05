
#### Methods for SparseImagingExperiment ####
## ------------------------------------------

SparseImagingExperiment <- function(imageData = matrix(nrow=0, ncol=0),
	featureData = DataFrame(), pixelData = PositionDataFrame(),
	metadata = list(), processing = SimpleList())
{
	if ( !is(imageData, "ImageArrayList") )
		imageData <- ImageArrayList(imageData)
	if ( length(imageData) != 0L ) {
		iData <- imageData[[1]]
		if ( missing(featureData) ) {
			rownames <- rownames(iData)
			featureData <- new("DataFrame", nrows=nrow(iData),
				rownames=rownames)
		}
		if ( missing(pixelData) ) {
			colnames <- colnames(iData)
			coord=expand.grid(x=seq_len(ncol(iData)), y=1L)
			pixelData <- PositionDataFrame(coord=coord,
				row.names=colnames)
		}
	}
	.SparseImagingExperiment(
		imageData=imageData,
		featureData=featureData,
		elementMetadata=pixelData,
		metadata=metadata,
		processing=processing)
}

.valid.SparseImagingExperiment <- function(object) {
	errors <- NULL
	if ( length(object@imageData) != 0L ) {
		if ( nrow(object@imageData) != nrow(object@featureData) )
			errors <- c(errors , paste("number of rows in 'featureData'",
				"must match number of rows in 'imageData'"))
		if ( ncol(object@imageData) != nrow(object@elementMetadata) )
			errors <- c(errors , paste("number of rows in 'pixelData'",
				"must match number of columns in 'imageData'"))
	}
	if ( is.null(errors) ) TRUE else errors
}

setValidity("SparseImagingExperiment", .valid.SparseImagingExperiment)

## position methods

setMethod("run", "SparseImagingExperiment",
	function(object) run(pixelData(object)))

setReplaceMethod("run", "SparseImagingExperiment",
	function(object, value) {
		run(pixelData(object)) <- value
		if ( validObject(object) )
			object
	})

setMethod("runNames", "SparseImagingExperiment",
	function(object) runNames(pixelData(object)))

setReplaceMethod("runNames", "SparseImagingExperiment",
	function(object, value) {
		runNames(pixelData(object)) <- value
		if ( validObject(object) )
			object
	})

setMethod("coord", "SparseImagingExperiment",
	function(object) coord(pixelData(object)))

setReplaceMethod("coord", "SparseImagingExperiment",
	function(object, value) {
		coord(pixelData(object)) <- value
		object
	})

setMethod("coordLabels", "SparseImagingExperiment",
	function(object) coordnames(object))

setReplaceMethod("coordLabels", "SparseImagingExperiment",
	function(object, value) {
		coordnames(object) <- value
		object
	})

setMethod("coordinates", "SparseImagingExperiment",
	function(obj, ...) coordinates(pixelData(object)))

setReplaceMethod("coordinates", "SparseImagingExperiment",
	function(object, value) {
		coordinates(pixelData(object)) <- value
		if ( validObject(object) )
			object
	})

setMethod("coordnames", "SparseImagingExperiment",
	function(x) coordnames(pixelData(x)))

setReplaceMethod("coordnames", "SparseImagingExperiment",
	function(x, value) {
		coordnames(pixelData(x)) <- value
		x
	})

setMethod("gridded", "SparseImagingExperiment",
	function(obj) gridded(pixelData(obj)))

setReplaceMethod("gridded", "SparseImagingExperiment",
	function(obj, value) {
		gridded(pixelData(obj)) <- value
		obj
	})

setMethod("resolution", "SparseImagingExperiment",
	function(object) resolution(pixelData(object)))

setReplaceMethod("resolution", "SparseImagingExperiment",
	function(object, value) {
		resolution(pixelData(object)) <- value
		object
	})

setMethod("dims", "SparseImagingExperiment",
	function(object) dims(pixelData(object)))

# processing methods

setMethod("processingData", "SparseImagingExperiment",
	function(object) object@processing)

setReplaceMethod("processingData", "SparseImagingExperiment",
	function(object, value) {
		object@processing <- value
		object
	})

## Subsetting

setMethod("[", "SparseImagingExperiment",
	function(x, i, j, ..., drop = FALSE) {
		if ( missing(i) ) {
			i <- seq_len(nrow(x))
		} else {
			i2 <- seq_len(nrow(x))
			i <- setNames(i2, rownames(x))[i]
		}
		if ( missing(j) ) {
			j <- seq_len(ncol(x))
		} else {
			j2 <- seq_len(ncol(x))
			j <- setNames(j2, colnames(x))[j]
		}
		x@imageData <- x@imageData[i,j,drop=NULL]
		x@featureData <- x@featureData[i,,drop=FALSE]
		x@elementMetadata <- x@elementMetadata[j,,drop=FALSE]
		x
	})

## rbind/cbind

setMethod("rbind", "SparseImagingExperiment",
	function(..., deparse.level=1)
	{
		objects <- unname(list(...))
		imageData <- do.call("rbind", lapply(objects, "imageData"))
		featureData <- do.call("rbind", lapply(objects, "featureData"))
		pixelData <- do.call("cbind", lapply(objects, "pixelData"))
		metadata <- do.call("c", lapply(objects, "metadata"))
		metadata <- metadata[unique(names(metadata))]
		needs_processing <- sapply(objects,
			function(obj) length(obj@processing) > 0L)
		if ( any(needs_processing) )
			.warning("dropping processing data")
		new(class(objects[[1L]]),
			imageData=imageData,
			featureData=featureData,
			elementMetadata=pixelData,
			metadata=metadata,
			processing=SimpleList())        
    }
)

setMethod("cbind", "SparseImagingExperiment",
	function(..., deparse.level=1)
	{
		objects <- unname(list(...))
		imageData <- do.call("cbind", lapply(objects, "imageData"))
		featureData <- do.call("cbind", lapply(objects, "featureData"))
		pixelData <- do.call("rbind", lapply(objects, "pixelData"))
		metadata <- do.call("c", lapply(objects, "metadata"))
		metadata <- metadata[unique(names(metadata))]
		needs_processing <- sapply(objects,
			function(obj) length(obj@processing) > 0L)
		if ( any(needs_processing) )
			.warning("dropping processing data")
		new(class(objects[[1L]]),
			imageData=imageData,
			featureData=featureData,
			elementMetadata=pixelData,
			metadata=metadata,
			processing=SimpleList())  
    }
)

