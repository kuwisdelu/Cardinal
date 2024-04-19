
#### MSImagingExperiment ####
## ---------------------------

# Class for a MS-based imaging experiment
# _with_ aligned feature information

MSImagingExperiment <- function(spectraData = SimpleList(),
	featureData = MassDataFrame(), pixelData = PositionDataFrame(),
	experimentData = NULL, centroided = NA, metadata = list())
{
	spectraData <- SpectraArrays(spectraData)
	if ( length(spectraData) != 0L )
	{
		spectra <- spectraData[[1L]]
		if ( missing(featureData) )
		{
			rownames <- rownames(spectra)
			mz <- seq_len(nrow(spectra))
			featureData <- MassDataFrame(mz=mz, row.names=rownames)
		}
		if ( missing(pixelData) )
		{
			colnames <- colnames(spectra)
			coord <- expand.grid(x=seq_len(ncol(spectra)), y=1L)
			pixelData <- PositionDataFrame(coord=coord, row.names=colnames)
		}
	}
	new("MSImagingExperiment", spectraData=spectraData,
		featureData=featureData, elementMetadata=pixelData,
		experimentData=experimentData, centroided=centroided,
		metadata=metadata, processing=list())
}

.valid_MSImagingExperiment <- function(object)
{
	errors <- NULL
	if ( length(object@centroided) != 1L )
		errors <- c(errors, "centroided must be a scalar logical")
	if ( is.null(errors) ) TRUE else errors
}

setValidity("MSImagingExperiment", .valid_MSImagingExperiment)

setMethod("show", "MSImagingExperiment",
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
		# mz()
		if ( length(object) > 0L )
		{
			mzr <- format(range(mz(object)))
			cat("mass range:", mzr[1L], "to", mzr[2L], "\n")
		}
		# centroided()
		cat("centroided:", centroided(object), "\n")
	})

## Getters and setters

# features

setMethod("features", "MSImagingExperiment",
	function(object, ..., mz, tolerance = NA, units = c("ppm", "mz"),
		env = NULL)
	{
		if ( is.null(env) )
			env <- parent.frame(2)
		if ( missing(units) && !missing(tolerance) )
			units <- get_units_from_names(tolerance, units)
		units <- match.arg(units)
		i <- callNextMethod(object, ..., env=env)
		if ( !missing(mz) && !is.null(mz) ) {
			ref <- switch(units, ppm="x", mz="abs")
			if ( is.na(tolerance) ) {
				tol <- estres(mz(object), ref=ref)
			} else {
				tol <- switch(units, ppm=1e-6 * tolerance, mz=tolerance)
			}
			i_mz <- bsearch(mz, mz(object), tol=tol, tol.ref=ref)
			if ( anyNA(i_mz) )
			{
				for ( j in which(is.na(i_mz)) )
				{
					k <- bsearch(mz[j], mz(object), nearest=TRUE)
					warning("no match for mz ", round(mz[j], digits=4L), "; ",
						"nearest is ", round(mz(object)[k], digits=4L))
				}
			}
			i <- intersect(i_mz, i)
		}
		setNames(i, featureNames(object)[i])
	})

get_units_from_names <- function(x, units)
{
	if ( !is.null(names(x)) && names(x) %in% units )
		units <- names(x)
	units
}


# mz

setMethod("mz", "missing",
	function(from, to, by, units = c("ppm", "mz"), ...)
	{
		units <- match.arg(units)
		by <- unname(by)
		mz <- switch(units,
			ppm=seq_rel(from=from, to=to, by=1e-6 * by),
			mz=seq.default(from=from, to=to, by=by))
		switch(units,
			ppm=structure(mz, resolution=c(ppm=by)),
			mz=structure(mz, resolution=c(mz=by)))
	})

setMethod("mz", "MSImagingExperiment",
	function(object, ...) mz(object@featureData))
setReplaceMethod("mz", "MSImagingExperiment",
	function(object, ..., value) {
		mz(object@featureData) <- value
		object
	})

# intensity

setMethod("intensity", "MSImagingExperiment",
	function(object, ...) object@spectraData[["intensity"]])
setReplaceMethod("intensity", "MSImagingExperiment",
	function(object, ..., value) {
		object@spectraData[["intensity"]] <- value
		object
	})

# centroided

setMethod("centroided", "MSImagingExperiment_OR_Arrays",
	function(object, ...) object@centroided)
setReplaceMethod("centroided", "MSImagingExperiment_OR_Arrays",
	function(object, ..., value) {
		object@centroided <- value
		object
	})

setMethod("isCentroided", "MSImagingExperiment_OR_Arrays",
	function(object, ...) isTRUE(object@centroided))

# experimentData

setMethod("experimentData", "MSImagingExperiment_OR_Arrays",
	function(object) object@experimentData)
setReplaceMethod("experimentData", "MSImagingExperiment_OR_Arrays",
	function(object, value) {
		object@experimentData <- value
		object
	})

## cbind / rbind

.cbind_MSImagingExperiment <- function(objects)
{
	spectraData <- do.call(cbind, lapply(objects, spectraData))
	featureData <- do.call(cbind, lapply(objects, featureData))
	pixelData <- do.call(rbind, lapply(objects, pixelData))
	centroided <- all(vapply(objects, centroided, logical(1L)))
	metadata <- do.call(c, lapply(objects, metadata))
	new(class(objects[[1L]]),
		spectraData=spectraData,
		featureData=featureData,
		elementMetadata=pixelData,
		experimentData=experimentData(objects[[1L]]),
		centroided=centroided,
		metadata=metadata,
		processing=list())
}

setMethod("cbind", "MSImagingExperiment",
	function(..., deparse.level = 1) .cbind_MSImagingExperiment(list(...)))

.rbind_MSImagingExperiment <- function(objects)
{
	spectraData <- do.call(rbind, lapply(objects, spectraData))
	featureData <- do.call(rbind, lapply(objects, featureData))
	pixelData <- do.call(cbind, lapply(objects, pixelData))
	centroided <- all(vapply(objects, centroided, logical(1L)))
	metadata <- do.call(c, lapply(objects, metadata))
	new(class(objects[[1L]]),
		spectraData=spectraData,
		featureData=featureData,
		elementMetadata=pixelData,
		experimentData=experimentData(objects[[1L]]),
		centroided=centroided,
		metadata=metadata,
		processing=list())
}

setMethod("rbind", "MSImagingExperiment",
	function(..., deparse.level = 1) .rbind_MSImagingExperiment(list(...)))

## Coercion

convertMSImagingExperiment2Arrays <- function(object)
{
	if ( is(object, "MSImagingArrays") )
		return(object)
	if ( !is(object, "MSImagingExperiment") )
		stop("object must be of class MSImagingExperiment")
	if ( is(spectra(object), "sparse_mat") ) {
		mz <- atomindex(spectra(object))
		intensity <- atomdata(spectra(object))
		continuous <- FALSE
	} else {
		mz <- rep.int(list(mz(object)), ncol(object))
		if ( is.sparse(spectra(object)) ) {
			intensity <- atomdata(spectra(object))
		} else if ( is.matter(spectra(object)) ) {
			intensity <- as(spectra(object), "matter_list")
		} else {
			intensity <- apply(spectra(object), 2L, identity, simplify=FALSE)
		}
		continuous <- TRUE
	}
	MSImagingArrays(list(mz=mz, intensity=intensity),
		pixelData=pixelData(object),
		experimentData=experimentData(object),
		metadata=metadata(object),
		centroided=centroided(object),
		continuous=continuous)
}

convertMSImagingArrays2Experiment <- function(object, mz = NULL,
	mass.range = NULL, resolution = NA, units = c("ppm", "mz"),
	guess.max = 1000L, tolerance = 0.5 * resolution,
	nchunks = getCardinalNChunks(),
	verbose = getCardinalVerbose(),
	BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( is(object, "MSImagingExperiment") )
		return(object)
	if ( !is(object, "MSImagingArrays") )
		stop("object must be of class MSImagingArrays")
	if ( missing(units) )
	{
		if ( !missing(resolution) ) {
			units <- get_units_from_names(resolution, units)
		} else if ( !missing(tolerance) ) {
			units <- get_units_from_names(tolerance, units)
		}
	}
	units <- match.arg(units)
	guess.max <- min(guess.max, length(object))
	if ( isTRUE(object@continuous) ) {
		# lossless coversion
		if ( is.null(mz) )
			mz <- mz(object)[[1L]]
		if ( is.matter(intensity(object)) ) {
			spectra <- as(intensity(object), "matter_mat")
		} else {
			spectra <- do.call(cbind, intensity(object))
		}
		featureData <- MassDataFrame(mz=mz)
		ans <- MSImagingExperiment(spectra,
			featureData=featureData,
			pixelData=pixelData(object),
			experimentData=experimentData(object),
			metadata=metadata(object),
			centroided=centroided(object))
	} else if ( !is.null(mz) ) {
		# reference m/z axis
		if ( is.na(resolution) ) {
			res.ref <- switch(units, ppm="relative", mz="absolute")
			resolution <- estres(mz, ref=res.ref)
			resolution <- switch(units,
				ppm=1e6 * resolution,
				mz=resolution)
		}
		ans <- bin(object, ref=mz, method="max",
			resolution=resolution, units=units)
	} else if ( isCentroided(object) ) {
		# centroid m/z axis
		ref <- object
		if ( is.finite(guess.max) ) {
			i <- seq(1L, length(object), length.out=guess.max)
			ref <- ref[i]
		}
		if ( verbose )
			message("estimating centroid m/z-axis from ",
				guess.max, " sample spectra")
		ref <- peakAlign(ref, ref=NULL,
			tolerance=tolerance, units=units,
			BPPARAM=BPPARAM)
		if ( is.null(mass.range) )
			mass.range <- round(range(mz(ref)), digits=4L)
		if ( is.na(tolerance) ) {
			tolerance <- tolerance(spectra(ref))
			tolerance <- switch(units, ppm=1e6 * tolerance, mz=tolerance)
		}
		if ( any(mz(ref) < min(mass.range)) )
			ref <- ref[mz(ref) < min(mass.range),]
		if ( any(mz(ref) > max(mass.range)) )
			ref <- ref[mz(ref) > max(mass.range),]
		if ( verbose ) {
			message("applying centroid m/z-values to all spectra")
			message("using mass.range ", mass.range[1L], " to ", mass.range [2L])
			message("using tolerance ", tolerance, " ", units)
		}
		ans <- peakAlign(object, ref=mz(ref),
			tolerance=tolerance, units=units,
			BPPARAM=BPPARAM)
	} else {
		# profile m/z axis
		mzlist <- mz(object)
		if ( is.finite(guess.max) ) {
			i <- seq(1L, length(object), length.out=guess.max)
			mzlist <- mzlist[i]
		}
		if ( is.null(mass.range) || is.na(resolution) )
		{
			if ( verbose )
				message("estimating profile m/z-axis from ",
					guess.max, " sample spectra")
			mz <- estimateDomain(mzlist,
				units=switch(units, ppm="relative", mz="absolute"),
				BPPARAM=BPPARAM)
			if ( is.null(mass.range) )
				mass.range <- round(range(mz), digits=4L)
			if ( is.na(resolution) )
				resolution <- 1e6 * attr(mz, "resolution")
		}
		mz <- mz(from=min(mass.range), to=max(mass.range),
			by=resolution, units=units)
		if ( verbose ) {
			message("applying profile m/z-values to all spectra")
			message("using mass.range ", mass.range[1L], " to ", mass.range [2L])
			message("using resolution ", resolution, " ", units)
		}
		ans <- bin(object, ref=mz, method="linear",
			resolution=resolution, units=units)
	}
	if ( validObject(ans) )
		ans
}

setAs("MSImagingArrays", "MSImagingExperiment",
	function(from) convertMSImagingArrays2Experiment(from))

setAs("MSImagingExperiment", "MSImagingArrays",
	function(from) convertMSImagingExperiment2Arrays(from))

setMethod("updateObject", "MSImagingExperiment",
	function(object, ..., verbose = FALSE)
	{
		if ( .hasSlot(object, "spectraData") ) {
			return(object)
		} else if ( .hasSlot(object, "imageData") ) {
			spectra <- object@imageData$data[[1L]]
			pixelData <- updateObject(object@elementMetadata)
			featureData <- updateObject(object@featureData)
			if ( is(object@metadata[["parse"]], "ImzML") ) {
				experimentData <- as(object@metadata[["parse"]], "ImzMeta")
			} else {
				experimentData <- NULL
			}
			MSImagingExperiment(spectra,
				featureData=featureData,
				pixelData=pixelData,
				experimentData=experimentData,
				metadata=object@metadata,
				centroided=object@centroided)
		} else {
			stop("don't know how to update this MSImagingExperiment instance")
		}
	})


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

