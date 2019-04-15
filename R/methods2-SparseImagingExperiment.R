
#### Methods for SparseImagingExperiment ####
## ------------------------------------------

SparseImagingExperiment <- function(imageData = matrix(nrow=0, ncol=0),
	featureData = XDataFrame(), pixelData = PositionDataFrame(),
	metadata = list(), processing = SimpleList())
{
	if ( !is(imageData, "ImageArrayList") )
		imageData <- ImageArrayList(imageData)
	if ( length(imageData) != 0L ) {
		iData <- imageData[[1]]
		if ( missing(featureData) ) {
			rownames <- rownames(iData)
			featureData <- new("XDataFrame", nrows=nrow(iData),
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
	function(obj, ...) coordinates(pixelData(obj)))

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
	function(x) dims(pixelData(x)))

# processing methods

setMethod("processingData", "SparseImagingExperiment",
	function(object) object@processing)

setReplaceMethod("processingData", "SparseImagingExperiment",
	function(object, value) {
		object@processing <- value
		object
	})

setMethod("preproc", "SparseImagingExperiment",
	function(object) {
		p <- mcols(object@processing)
		setNames(p$complete, p$label)
	})

## Subsetting

setMethod("[", "SparseImagingExperiment",
	function(x, i, j, ..., drop) {
		if ( !missing(i) && (is.character(i) || is.factor(i)) )
			i <- match(i, featureNames(x))
		if ( !missing(j) && (is.character(j) || is.factor(j)) )
			j <- match(j, pixelNames(x))
		if ( !missing(i) && !missing(j) ) {
			x@imageData <- x@imageData[i,j,drop=NULL]
			x@featureData <- x@featureData[i,,drop=FALSE]
			x@elementMetadata <- x@elementMetadata[j,,drop=FALSE]	
		} else if ( !missing(i) ) {
			x@imageData <- x@imageData[i,,drop=NULL]
			x@featureData <- x@featureData[i,,drop=FALSE]
		} else if ( !missing(j) ) {
			x@imageData <- x@imageData[,j,drop=NULL]
			x@elementMetadata <- x@elementMetadata[j,,drop=FALSE]
		}
		x
	})

## combine

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

setMethod("combine", "SparseImagingExperiment",
	function(x, y, ...) cbind(x, y, ...))

## show

setMethod("show", "SparseImagingExperiment",
	function(object) {
		# check for processing
		.checkForIncompleteProcessing(object, message.only=TRUE)
		# print parent information
		callNextMethod(object)
		# processingData()
		if ( length(processingData(object)) > 0L ) {
			plist <- mcols(processingData(object))
			complete <- plist$label[plist$complete]
			.scat("processing complete(%d): %s\n", complete,
				prefix="    ")
			pending <- plist$label[plist$pending]
			.scat("processing pending(%d): %s\n", pending,
				prefix="    ")
		}
		# raster dims()
		rdims <- paste0(dims(object), collapse=" x ")
		.scat("raster dimensions(%d): %s\n", rdims,
			collapse=", ", prefix="    ")
		# coord
		clims <- sapply(coord(object), function(ci) paste0(range(ci), collapse=".."))
		clims <- paste0(paste0(names(coord(object)), sep=" = "), clims)
		.scat("coord(%d): %s\n", clims,
			collapse=", ", prefix="    ")
	}
)

