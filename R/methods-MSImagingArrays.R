
#### MSImagingArrays ####
## ----------------------

# Class for a list of (unprocessed) mass spectra
# _without_ any aligned feature information

MSImagingArrays <- function(spectraData = SimpleList(),
	pixelData = PositionDataFrame(), experimentData = NULL,
	centroided = NA, continuous = NA, metadata = list())
{
	spectraData <- SpectraArrays(spectraData)
	if ( length(spectraData) != 0L )
	{
		if ( missing(pixelData) )
		{
			names <- names(spectraData[[1L]])
			coord <- expand.grid(x=seq_len(nrow(spectraData)), y=1L)
			pixelData <- PositionDataFrame(coord=coord, row.names=names)
		}
	}
	new("MSImagingArrays", spectraData=spectraData,
		elementMetadata=pixelData, experimentData=experimentData,
		centroided=centroided, continuous=continuous,
		metadata=metadata, processing=list())
}

.valid_MSImagingArrays <- function(object)
{
	errors <- NULL
	if ( length(object@centroided) != 1L )
		errors <- c(errors, "centroided must be a scalar logical")
	if ( length(object@continuous) != 1L )
		errors <- c(errors, "continuous must be a scalar logical")
	if ( length(object@spectraData) > 0L )
	{
		if ( !"mz" %in% names(object@spectraData) )
			errors <- c(errors, "spectraData must include an array named 'mz'")
		if ( !"intensity" %in% names(object@spectraData) )
			errors <- c(errors, "spectraData must include an array named 'intensity'")
	}
	if ( is.null(errors) ) TRUE else errors
}

setValidity("MSImagingArrays", .valid_MSImagingArrays)

setMethod("show", "MSImagingArrays",
	function(object) {
		callNextMethod()
		# experimentData()
		if ( !is.null(experimentData(object)) )
		{
			exp <- as.list(experimentData(object))
			exp <- names(exp[lengths(exp) > 0L])
			cat(sprintf("experimentData(%d): %s\n",
				length(exp), .paste_head_tail(exp)))
		}
		# centroided()
		cat("centroided:", centroided(object), "\n")
		# continuous
		cat("continuous:", object@continuous, "\n")
	})

## Getters and setters

# mz

setMethod("mz", "MSImagingArrays",
	function(object, i = NULL, ...) {
		if ( is.null(i) ) {
			object@spectraData[["mz"]]
		} else {
			object@spectraData[["mz"]][[i]]
		}
	})
setReplaceMethod("mz", "MSImagingArrays",
	function(object, i = NULL, ..., value) {
		if ( is.null(i) ) {
			object@spectraData[["mz"]] <- value
		} else {
			object@spectraData[["mz"]][[i]] <- value
		}
		object
	})

# intensity

setMethod("intensity", "MSImagingArrays",
	function(object, i = NULL, ...) {
		if ( is.null(i) ) {
			object@spectraData[["intensity"]]
		} else {
			object@spectraData[["intensity"]][[i]]
		}
	})
setReplaceMethod("intensity", "MSImagingArrays",
	function(object, i = NULL, ..., value) {
		if ( is.null(i) ) {
			object@spectraData[["intensity"]] <- value
		} else {
			object@spectraData[["intensity"]][[i]] <- value
		}
		object
	})

## combine

.combine_MSImagingArrays <- function(objects)
{
	spectraData <- do.call(c, lapply(objects, spectraData))
	pixelData <- do.call(rbind, lapply(objects, pixelData))
	centroided <- all(vapply(objects, slot, logical(1L), name="centroided"))
	continuous <- all(vapply(objects, slot, logical(1L), name="continuous"))
	metadata <- do.call(c, lapply(objects, metadata))
	new(class(objects[[1L]]),
		spectraData=spectraData,
		elementMetadata=pixelData,
		experimentData=experimentData(objects[[1L]]),
		centroided=centroided,
		continuous=continuous,
		metadata=metadata,
		processing=list())
}

setMethod("c", "MSImagingArrays",
	function(x, ...) .combine_MSImagingArrays(list(x, ...)))

