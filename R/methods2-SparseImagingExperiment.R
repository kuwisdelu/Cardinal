
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
			featureData <- new("DFrame", nrows=nrow(iData),
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


## elementMetadata methods

setMethod("pixelData", "SparseImagingExperiment", 
	function(object) object@elementMetadata)

setReplaceMethod("pixelData", "SparseImagingExperiment",
	function(object, value) {
		object@elementMetadata <- value
		if ( validObject(object) )
			object
	})

setMethod("pixelNames", "SparseImagingExperiment",
	function(object) rownames(elementMetadata(object)))

setReplaceMethod("pixelNames", "SparseImagingExperiment",
	function(object, value) {
		rownames(elementMetadata(object)) <- value
		object
	})

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

setMethod("is3D", "SparseImagingExperiment",
	function(object) is3D(pixelData(object)))

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


## Filter pixels/features

setMethod("features", "SparseImagingExperiment",
	function(object, ..., .env = parent.frame(1)) {
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

setMethod("pixels", "SparseImagingExperiment",
	function(object, ..., .env = parent.frame(1)) {
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

setMethod("[", "SparseImagingExperiment",
	function(x, i, j, ..., drop) {
		lst <- (nargs() - !missing(drop)) < 3L
		if ( lst )
			return(x[,i,drop=drop])
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

## pull data into memory

# setMethod("pull", "SparseImagingExperiment", function(x, ...)
# 	{
# 		data <- as(imageData(x), "SimpleList", strict=FALSE)
# 		imageData(x) <- as(endoapply(data, as.matrix), class(imageData(x)))
# 		if ( validObject(x) )
# 			x
# 	})

## coerce to DataFrame

setAs("SparseImagingExperiment", "DataFrame",
	function(from) {
		nm <- names(imageData(from))[[1L]]
		if ( ncol(from) > 1L )
			nm <- paste0(nm, ".", seq_len(ncol(from)))
		data <- lapply(seq_len(ncol(from)),
			function(i) as.numeric(iData(from)[,i]))
		fData <- featureData(from)
		fData[nm] <- data
		fData
	})

## show

setMethod("show", "SparseImagingExperiment",
	function(object) {
		# check for processing
		.checkForIncompleteProcessing(object, message.only=TRUE)
		# print parent information
		callNextMethod(object)
		# processingData()
		t1 <- "    "
		if ( length(processingData(object)) > 0L ) {
			plist <- mcols(processingData(object))
			complete <- plist$label[plist$complete]
			.scat("processing complete(%d): %s\n", complete, prefix=t1)
			pending <- plist$label[plist$pending]
			.scat("processing pending(%d): %s\n", pending, prefix=t1)
		}
		# run()
		.scat("run(%d): %s\n", runNames(object), prefix=t1)
		# raster dims()
		rdims <- paste0(dims(object), collapse=" x ")
		cat(t1, "raster dimensions: ", rdims, "\n", sep="")
		# coord()
		clims <- sapply(coord(object), function(ci) paste0(range(ci), collapse=".."))
		clims <- paste0(paste0(names(coord(object)), sep=" = "), clims)
		.scat("coord(%d): %s\n", clims, collapse=", ", prefix=t1)
	}
)

## Additional methods

setMethod("dim", "SparseImagingExperiment", function(x)
	setNames(callNextMethod(x), c("Features", "Pixels")))



