
#### MSImagingArrays ####
## ----------------------

# Class for a list of (unprocessed) mass spectra
# _without_ any aligned feature information

MSImagingArrays <- function(spectraData = SimpleList(),
	pixelData = PositionDataFrame(), metadata = list(),
	centroided = NA, continuous = NA, experimentData = NULL)
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
	new("MSImagingArrays",
		spectraData=spectraData,
		elementMetadata=pixelData,
		experimentData=experimentData,
		centroided=centroided,
		continuous=continuous,
		metadata=metadata,
		processingQueue=list(),
		processingVariables=character(),
		processingChunkSize=NA_integer_)
}

.valid_MSImagingArrays <- function(object)
{
	errors <- NULL
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
			spectra(object, "mz", ...)
		} else {
			.Deprecated(old="i")
			spectra(object, "mz", ...)[[i]]
		}
	})
setReplaceMethod("mz", "MSImagingArrays",
	function(object, i = NULL, ..., value) {
		if ( is.null(i) ) {
			spectra(object, "mz", ...) <- value
		} else {
			.Deprecated(old="i")
			spectra(object, "mz", ...)[[i]] <- value
		}
		object
	})

# intensity

setMethod("intensity", "MSImagingArrays",
	function(object, i = NULL, ...) {
		if ( is.null(i) ) {
			spectra(object, "intensity", ...)
		} else {
			.Deprecated(old="i")
			spectra(object, "intensity", ...)[[i]]
		}
	})
setReplaceMethod("intensity", "MSImagingArrays",
	function(object, i = NULL, ..., value) {
		if ( is.null(i) ) {
			spectra(object, "intensity", ...) <- value
		} else {
			.Deprecated(old="i")
			spectra(object, "intensity", ...)[[i]] <- value
		}
		object
	})

# experimentData

setMethod("experimentData", "MSImagingArrays",
	function(object) object@experimentData)
setReplaceMethod("experimentData", "MSImagingArrays",
	function(object, value) {
		object@experimentData <- value
		if ( validObject )
			object
	})

