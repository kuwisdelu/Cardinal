
#### SpectralImagingExperiment ####
## --------------------------------

# Class for spectral imaging experiment
# _with_ aligned feature information
# following DataFrame semantics (length is # of columns/spectra)

.valid_SpectralImagingExperiment <- function(object)
{
	errors <- NULL
	if ( length(object@spectraData) > 0L )
	{
		nr_spectra <- nrow(object@spectraData)
		nc_spectra <- ncol(object@spectraData)
		nr_featureData <- nrow(object@featureData)
		nr_pixelData <- nrow(object@elementMetadata)
		if ( nr_spectra != nr_featureData )
			errors <- c(errors, paste0("number of rows in spectraData [",
				nr_spectra, "] must match number of rows in featureData [",
				nr_featureData, "]"))
		if ( nc_spectra != nr_pixelData )
			errors <- c(errors, paste0("number of columns in spectraData [",
				nc_spectra, "] must match number of rows in pixelData [",
				nr_pixelData, "]"))
	}
	if ( is.null(errors) ) TRUE else errors
}

setValidity("SpectralImagingExperiment", .valid_SpectralImagingExperiment)

SpectralImagingExperiment <- function(spectraData = SimpleList(),
	featureData = DataFrame(), pixelData = PositionDataFrame(),
	metadata = list())
{
	spectraData <- SpectraArrays(spectraData)
	if ( length(spectraData) != 0L )
	{
		spectra <- spectraData[[1L]]
		if ( missing(featureData) )
		{
			rownames <- rownames(spectra)
			featureData <- new("DFrame", nrows=nrow(spectra), rownames=rownames)
		}
		if ( missing(pixelData) )
		{
			colnames <- colnames(spectra)
			coord <- expand.grid(x=seq_len(ncol(spectra)), y=1L)
			pixelData <- PositionDataFrame(coord=coord, row.names=colnames)
		}
	}
	new("SpectralImagingExperiment", spectraData=spectraData,
		featureData=featureData, elementMetadata=pixelData,
		metadata=metadata, processing=list())
}

setMethod("show", "SpectralImagingExperiment",
	function(object) {
		# dimensions
		label <- if(length(object) != 1L) "spectra" else "spectrum"
		cat(class(object), "with", nrow(object), "features",
			"and", length(object), label, "\n")
		# spectraData()
		cat(sprintf("spectraData(%d): %s\n", length(spectraData(object)),
			.paste_head_tail(names(spectraData(object)))))
		# featureData()
		cat(sprintf("featureData(%d): %s\n", length(featureData(object)),
			.paste_head_tail(names(featureData(object)))))
		# pixelData()
		cat(sprintf("pixelData(%d): %s\n", length(pixelData(object)),
			.paste_head_tail(names(pixelData(object)))))
		# coord()
		if ( length(object) > 0L )
		{
			lims <- vapply(coord(object), range, numeric(2L))
			lims <- paste0(coordNames(object), " = ", lims[1L,], "...", lims[2L,])
			cat(sprintf("coord(%d): %s\n", length(coordNames(object)),
				.paste_head_tail(lims)))
		}
		# runNames()
		cat(sprintf("runNames(%d): %s\n", length(runNames(object)),
			.paste_head_tail(runNames(object))))
		# processingData()
		.print_queued_processing(object)
		# metadata()
		if ( length(metadata(object)) > 0L )
		{
			cat(sprintf("metadata(%d): %s\n", length(metadata(object)),
				.paste_head_tail(names(metadata(object)))))
		}
	})

# find pixels by position
setMethod("pixels", "SpectralImagingExperiment",
	function(object, ..., coord, run, tolerance = NA,
		env = NULL)
	{
		if ( is.null(env) )
			env <- parent.frame(2)
		i <- callNextMethod(object, ..., env=env)
		if ( !missing(coord) || !missing(run) ) {
			pos <- .find_positions(object, coord, run, tolerance)
			i <- intersect(i, pos)
		}
		setNames(i, pixelNames(object)[i])
	})

## Slot getters and setters

# featureData

setMethod("featureData", "SpectralImagingExperiment",
	function(object) object@featureData)
setReplaceMethod("featureData", "SpectralImagingExperiment",
	function(object, value) {
		object@featureData <- value
		if ( validObject(object) )
			object
	})

setMethod("fData", "SpectralImagingExperiment",
	function(object) featureData(object))
setReplaceMethod("fData", "SpectralImagingExperiment",
	function(object, value) {
		featureData(object) <- value
		object
	})

setMethod("featureNames", "SpectralImagingExperiment",
	function(object) rownames(featureData(object)))
setReplaceMethod("featureNames", "SpectralImagingExperiment",
	function(object, value) {
		rownames(featureData(object)) <- value
			object
	})

setMethod("features", "SpectralImagingExperiment",
	function(object, ..., env = NULL)
	{
		if ( is.null(env) )
			env <- parent.frame(2)
		env <- as.env(featureData(object), enclos=env)
		conditions <- eval(substitute(alist(...)))
		i <- .find_conditions(conditions, env, nrow(featureData(object)))
		setNames(i, featureNames(object)[i])
	})

## Basic getters and setters

# note: we get dim() from RectangularData
setMethod("nrow", "SpectralImagingExperiment", function(x) nrow(featureData(x)))
setMethod("ncol", "SpectralImagingExperiment", function(x) length(x))

# note: we get dimnames() from RectangularData
setMethod("rownames", "SpectralImagingExperiment",
	function(x) rownames(featureData(x)))
setReplaceMethod("rownames", "SpectralImagingExperiment",
	function(x, value) {
		rownames(featureData(x)) <- value
		x
	})

setMethod("colnames", "SpectralImagingExperiment",
	function(x) rownames(pixelData(x)))
setReplaceMethod("colnames", "SpectralImagingExperiment",
	function(x, value) {
		rownames(pixelData(x)) <- value
		x
	})

## Array-like subsetting

.subset_SpectralImagingExperiment <- function(x, i, j)
{
	if ( !missing(i) && (is.character(i) || is.factor(i)) )
		i <- match(i, featureNames(x))
	if ( !missing(j) && (is.character(j) || is.factor(j)) )
		j <- match(j, pixelNames(x))
	if ( !missing(i) && !missing(j) ) {
		x@spectraData <- x@spectraData[i,j,drop=FALSE]
	} else if ( !missing(i) ) {
		x@spectraData <- x@spectraData[i,,drop=FALSE]
	} else if ( !missing(j) ) {
		x@spectraData <- x@spectraData[,j,drop=FALSE]
	}
	if ( !missing(i) )
		x@featureData <- x@featureData[i,,drop=FALSE]
	if ( !missing(j) )
		x@elementMetadata <- x@elementMetadata[j,,drop=FALSE]
	if ( validObject(x) )
		x
}

setMethod("[", "SpectralImagingExperiment",
	function(x, i, j, ..., drop = TRUE) {
		if ( (nargs() - !missing(drop)) < 3L )
			return(x[,i])
		if ( !missing(drop) && isTRUE(drop) )
			.Warn("'drop' ignored when subsetting ", class(x))
		.subset_SpectralImagingExperiment(x, i, j)
	})

setMethod("subset", "SpectralImagingExperiment",
	function(x, select, subset, ...)
	{
		pdata <- as.env(pixelData(x), enclos=parent.frame(2))
		fdata <- as.env(featureData(x), enclos=parent.frame(2))
		if ( !missing(select) ) {
			i <- eval(substitute(select), envir=fdata)
			if ( !is.logical(i) && !is.numeric(i) )
				.Error("'select' must specify logical or numeric indices")
		}
		if ( !missing(subset) ) {
			j <- eval(substitute(subset), envir=pdata)
			if ( !is.logical(j) && !is.numeric(j) )
				.Error("'subset' must specify logical or numeric indices")
		}
		if ( !missing(select) && !missing(subset) ) {
			x[i,j]
		} else if ( !missing(select) ) {
			x[i,]
		} else if ( !missing(subset) ) {
			x[,j]
		} else {
			x
		}
	})

subsetPixels <- function(x, ...) {
	x[,pixels(x, ..., env=parent.frame(1))]
}

subsetFeatures <- function(x, ...) {
	x[features(x, ..., env=parent.frame(1)),]
}


## cbind / rbind

.cbind_SpectralImagingExperiment <- function(objects)
{
	spectraData <- do.call(cbind, lapply(objects, spectraData))
	featureData <- do.call(cbind, lapply(objects, featureData))
	pixelData <- do.call(rbind, lapply(objects, pixelData))
	metadata <- do.call(c, lapply(objects, metadata))
	new(class(objects[[1L]]),
		spectraData=spectraData,
		featureData=featureData,
		elementMetadata=pixelData,
		metadata=metadata,
		processing=list())
}

setMethod("cbind", "SpectralImagingExperiment",
	function(..., deparse.level = 1) .cbind_SpectralImagingExperiment(list(...)))

.rbind_SpectralImagingExperiment <- function(objects)
{
	spectraData <- do.call(rbind, lapply(objects, spectraData))
	featureData <- do.call(rbind, lapply(objects, featureData))
	pixelData <- do.call(cbind, lapply(objects, pixelData))
	metadata <- do.call(c, lapply(objects, metadata))
	new(class(objects[[1L]]),
		spectraData=spectraData,
		featureData=featureData,
		elementMetadata=pixelData,
		metadata=metadata,
		processing=list())
}

setMethod("rbind", "SpectralImagingExperiment",
	function(..., deparse.level = 1) .rbind_SpectralImagingExperiment(list(...)))

