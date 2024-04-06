
#### SpectralImagingExperiment ####
## --------------------------------

# Class for a spectral imaging experiment
# _with_ aligned feature information

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
	if ( length(object@processing) > 0L )
	{
		ps_ok <- vapply(object@processing, is, logical(1L), class2="ProcessingStep")
		if ( !all(ps_ok) )
			errors <- c(errors, paste0("all elements of processing ",
				"must be ProcessingStep objects"))
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
		.print_pending_processing(object)
		# metadata()
		if ( length(metadata(object)) > 0L )
		{
			cat(sprintf("metadata(%d): %s\n", length(metadata(object)),
				.paste_head_tail(names(metadata(object)))))
		}
	})

# find pixels by position
setMethod("pixels", "SpectralImagingExperiment",
	function(object, ..., coord, run, tolerance = NA)
	{
		i <- callNextMethod(object, ...)
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
	function(object, ..., env = parent.frame(1L))
	{
		conditions <- eval(substitute(alist(...)))
		e <- as.env(featureData(object), enclos=env)
		i <- .find_conditions(conditions, e, nrow(featureData(object)))
		setNames(i, featureNames(object)[i])
	})

## Basic getters and setters

# note: we follow data frame arrangement (length is # of columns/spectra)
setMethod("length", "SpectralImagingExperiment", function(x) nrow(pixelData(x)))

setMethod("names", "SpectralImagingExperiment",
	function(x) colnames(x))
setReplaceMethod("names", "SpectralImagingExperiment",
	function(x, value) {
		colnames(x) <- value
		x
	})

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
			warning("'drop' ignored when subsetting ", class(x))
		.subset_SpectralImagingExperiment(x, i, j)
	})

setMethod("subset", "SpectralImagingExperiment",
	function(x, subset, select, ...)
	{
		pdata <- as.env(pixelData(x), enclos=parent.frame(2))
		fdata <- as.env(featureData(x), enclos=parent.frame(2))
		if ( !missing(subset) ) {
			i <- eval(substitute(subset), envir=fdata)
			if ( !is.logical(i) && !is.numeric(i) )
				stop("'subset' must specify logical or numeric indices")
		}
		if ( !missing(select) ) {
			j <- eval(substitute(select), envir=pdata)
			if ( !is.logical(i) && !is.numeric(i) )
				stop("'select' must specify logical or numeric indices")
		}
		if ( !missing(subset) && !missing(select) ) {
			x[i,j]
		} else if ( !missing(subset) ) {
			x[i,]
		} else if ( !missing(select) ) {
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



#### SpectralImagingArrays ####
## ----------------------------

# Class for a list of (unprocessed) spectra
# _without_ any aligned feature information

.valid_SpectralImagingArrays <- function(object)
{
	errors <- NULL
	if ( length(object@spectraData) > 0L )
	{
		nr_spectra <- nrow(object@spectraData)
		nr_pixelData <- nrow(object@elementMetadata)
		if ( length(dim(object@spectraData)) != 2L )
			errors <- c(errors, paste0("number of dimensions of spectraData [",
				length(dim(object@spectraData)), "] must be 2"))
		if ( nr_spectra != nr_pixelData )
			errors <- c(errors, paste0("number of rows in spectraData [",
				nr_spectra, "] must match number of rows in pixelData [",
				nr_pixelData, "]"))
	}
	if ( is.null(errors) ) TRUE else errors
}

setValidity("SpectralImagingArrays", .valid_SpectralImagingArrays)

SpectralImagingArrays <- function(spectraData = SimpleList(),
	pixelData = PositionDataFrame(), metadata = list())
{
	spectraData <- SpectraArrays(spectraData)
	if ( length(spectraData) != 0L )
	{
		spectra <- spectraData[[1L]]
		if ( missing(pixelData) )
		{
			colnames <- colnames(spectra)
			coord <- expand.grid(x=seq_len(length(spectra)), y=1L)
			pixelData <- PositionDataFrame(coord=coord, row.names=colnames)
		}
	}
	new("SpectralImagingArrays", spectraData=spectraData,
		elementMetadata=pixelData, metadata=metadata,
		processing=list())
}

.paste_head_tail <- function(x, n = 6L, collapse = ", ")
{
	paste0(.select_head_tail(x, n), collapse=collapse)
}

.print_pending_processing <- function(object)
{
	if ( length(processingData(object)) > 0L )
	{
		cat(sprintf("processingData(%d): %s\n", length(processingData(object)),
			.paste_head_tail(names(processingData(object)))))
		cat("NOTE: use process() to execute queued processing steps\n")
		cat("NOTE: use reset() to remove queued processing steps\n")
		cat("NOTE: use plot() to preview queued processing steps\n")
	}
}

setMethod("show", "SpectralImagingArrays",
	function(object) {
		# dimensions
		label <- if(length(object) != 1L) "spectra" else "spectrum"
		cat(class(object), "with", length(object), label, "\n")
		# spectraData()
		cat(sprintf("spectraData(%d): %s\n", length(spectraData(object)),
			.paste_head_tail(names(spectraData(object)))))
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
		.print_pending_processing(object)
		# metadata()
		if ( length(metadata(object)) > 0L )
		{
			cat(sprintf("metadata(%d): %s\n", length(metadata(object)),
				.paste_head_tail(names(metadata(object)))))
		}
	})

# find spectra by position
setMethod("pixels", "SpectralImagingArrays",
	function(object, ..., coord, run, tolerance = NA)
	{
		i <- callNextMethod(object, ...)
		if ( !missing(coord) || !missing(run) ) {
			pos <- .find_positions(object, coord, run, tolerance)
			i <- intersect(i, pos)
		}
		setNames(i, pixelNames(object)[i])
	})

.find_positions <- function(object, coord, run, tol = NA)
{
	index <- seq_len(nrow(pixelData(object)))
	if ( !missing(coord) ) {
		coord <- as.data.frame(as.list(coord))
		if ( is.na(tol) )
			tol <- vapply(coord(object), estres, numeric(1L))
		i_coord <- kdsearch(coord, coord(object), tol=tol)
		i_coord <- unique(unlist(i_coord))
		index <- intersect(i_coord, index)
	}
	if ( !missing(run) ) {
		if ( !is.character(run) && !is.factor(run) )
			run <- runNames(object)[run]
		i_run <- which(run(object) %in% run)
		index <- intersect(i_run, index)
	}
	index
}

## Basic getters and setters

# note: we follow vector arrangement (length is # of elements/spectra)
setMethod("length", "SpectralImagingArrays", function(x) nrow(pixelData(x)))

setMethod("names", "SpectralImagingArrays",
	function(x) rownames(pixelData(x)))
setReplaceMethod("names", "SpectralImagingArrays",
	function(x, value) {
		rownames(pixelData(x)) <- value
		x
	})

## Vector-like subsetting

.subset_SpectralImagingArrays <- function(x, i)
{
	if ( is.character(i) || is.factor(i) )
		i <- match(i, pixelNames(x))
	x@spectraData <- x@spectraData[i,,drop=FALSE]
	x@elementMetadata <- x@elementMetadata[i,,drop=FALSE]
	if ( validObject(x) )
		x
}

setMethod("[", "SpectralImagingArrays",
	function(x, i, j, ..., drop = TRUE) {
		if ( !missing(drop) && isTRUE(drop) )
			warning("'drop' ignored when subsetting ", class(x))
		.subset_SpectralImagingArrays(x, i)
	})

## combine

.combine_SpectralImagingArrays <- function(objects)
{
	spectraData <- do.call(c, lapply(objects, spectraData))
	pixelData <- do.call(rbind, lapply(objects, pixelData))
	metadata <- do.call(c, lapply(objects, metadata))
	new(class(objects[[1L]]),
		spectraData=spectraData,
		elementMetadata=pixelData,
		metadata=metadata,
		processing=list())
}

setMethod("c", "SpectralImagingArrays",
	function(x, ...) .combine_SpectralImagingArrays(list(x, ...)))


#### SpectralImagingData ####
## --------------------------

# VIRTUAL class for containing spectra arrays
# with imaging position/pixel information

.valid_SpectralImagingData <- function(object)
{
	errors <- NULL
	if ( length(object@processing) > 0L )
	{
		is_ps <- vapply(object@processing, is, logical(1L), "ProcessingStep")
		if ( !all(is_ps) )
			errors <- c(errors, paste0("all processing elements ",
				"must be ProcessingStep objects"))
	}
	if ( is.null(errors) ) TRUE else errors
}

setValidity("SpectralImagingData", .valid_SpectralImagingData)

# Make matter::mem() report virtual memory correctly
setMethod("vm_used", "SpectralImagingData",
	function(x) vm_used(spectraData(x)))

## Slot getters and setters

# spectraData

setMethod("spectraData", "SpectralImagingData",
	function(object, ...) object@spectraData)
setReplaceMethod("spectraData", "SpectralImagingData",
	function(object, ..., value) {
		object@spectraData <- SpectraArrays(value)
		if ( validObject(object) )
			object
	})

setMethod("spectraNames", "SpectralImagingData",
	function(object, ...) names(spectraData(object)))
setReplaceMethod("spectraNames", "SpectralImagingData",
	function(object, ..., value) {
		names(spectraData(object)) <- value
		object
	})

# Spectra array access

setMethod("spectra", "SpectralImagingData",
	function(object, i = 1L, ...) object@spectraData[[i]])
setReplaceMethod("spectra", "SpectralImagingData",
	function(object, i = 1L, ..., value) {
		object@spectraData[[i]] <- value
		object
	})

# pixelData

setMethod("pixelData", "SpectralImagingData",
	function(object) object@elementMetadata)
setReplaceMethod("pixelData", "SpectralImagingData",
	function(object, value) {
		object@elementMetadata <- value
		if ( validObject(object) )
			object
	})

setMethod("pData", "SpectralImagingData",
	function(object) pixelData(object))
setReplaceMethod("pData", "SpectralImagingData",
	function(object, value) {
		pixelData(object) <- value
		object
	})

setMethod("pixelNames", "SpectralImagingData",
	function(object) rownames(pixelData(object)))
setReplaceMethod("pixelNames", "SpectralImagingData",
	function(object, value) {
		rownames(pixelData(object)) <- value
		object
	})

setMethod("pixels", "SpectralImagingData",
	function(object, ..., env = parent.frame(1L))
	{
		conditions <- eval(substitute(alist(...)))
		e <- as.env(pixelData(object), enclos=env)
		i <- .find_conditions(conditions, e, nrow(pixelData(object)))
		setNames(i, pixelNames(object)[i])
	})

.find_conditions <- function(expr, env, n)
{
	if ( length(expr) == 0L )
		return(seq_len(n))
	FUN <- function(cond) {
		ci <- eval(cond, envir=env)
		if ( is.logical(ci) && length(ci) != n )
			stop("length of condition [", length(ci),
				"] must match extent of object [", n, "]")
		if ( is.numeric(ci) && any(ci < 1L | ci > n) )
			stop("subscript out of bounds")
		ci
	}
	ans <- lapply(expr, FUN)
	bools <- vapply(ans, is.logical, logical(1L))
	conditions <- do.call(cbind, ans[bools])
	i <- unique(unlist(ans[!bools]))
	if ( length(conditions) > 0L ) {
		trues <- apply(conditions, 1L, all)
		if ( length(i) > 0L ) {
			intersect(i, which(trues))
		} else {
			which(trues)
		}
	} else {
		i
	}
}

# Variable names

setMethod("spectraVariables", "SpectralImagingData",
	function(object, ...) names(pixelData(object)))

# Coord/Run access

setMethod("coord", "SpectralImagingData",
	function(object, ...) coord(pixelData(object)))
setReplaceMethod("coord", "SpectralImagingData",
	function(object, ..., value) {
		coord(pixelData(object)) <- value
		object
	})

setMethod("coordNames", "SpectralImagingData",
	function(object) coordNames(pixelData(object)))
setReplaceMethod("coordNames", "SpectralImagingData",
	function(object, value) {
		coordNames(pixelData(object)) <- value
		object
	})

setMethod("run", "SpectralImagingData",
	function(object, ...) run(pixelData(object)))
setReplaceMethod("run", "SpectralImagingData",
	function(object, ..., value) {
		run(pixelData(object)) <- value
		object
	})

setMethod("runNames", "SpectralImagingData",
	function(object) runNames(pixelData(object)))
setReplaceMethod("runNames", "SpectralImagingData",
	function(object, value) {
		runNames(pixelData(object)) <- value
		object
	})

setMethod("nrun", "SpectralImagingData",
	function(x) nrun(pixelData(x)))

# processingData

setMethod("processingData", "SpectralImagingData",
	function(object, ...) object@processing)
setReplaceMethod("processingData", "SpectralImagingData",
	function(object, ..., value) {
		object@processing <- value
		if ( validObject(object) )
			object
	})

## Basic getters and setters

# access spectra variables
setMethod("[[", "SpectralImagingData",
	function(x, i, j, ...) pixelData(x)[[i, ...]])

setReplaceMethod("[[", "SpectralImagingData",
	function(x, i, j, ..., value) {
		pixelData(x)[[i, ...]] <- value
		x
	})

setMethod("$", "SpectralImagingData",
	function(x, name) pixelData(x)[[name, exact=FALSE]])

setReplaceMethod("$", "SpectralImagingData",
	function(x, name, value) {
		pixelData(x)[[name]] <- value
		x
	})

# allow tab completion in console
.DollarNames.SpectralImagingData <- function(x, pattern = "") {
	grep(pattern, names(pixelData(x)), value=TRUE)
}

