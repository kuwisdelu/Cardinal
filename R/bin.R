
#### Spectral binning ####
## --------------------------

# MSImagingExperiment

setMethod("bin", "MSImagingExperiment",
	function(x, ref,
		spectra = "intensity", index = "mz",
		method = c("sum", "mean", "max", "min",
			"linear", "cubic", "gaussian", "lanczos"),
		resolution = NA, tolerance = NA, units = c("ppm", "mz"),
		mass.range = NULL, ...)
{
	if ( !is.null(mass.range) ) {
		if ( is.na(resolution) )
			.Error("setting 'mass.range' requires setting 'resolution'")
		ref <- mass.range
	}
	if ( !missing(ref) ) {
		if ( is(ref, "MSImagingExperiment") || is(ref, "MassDataFrame") )
			ref <- mz(ref)
	}
	if ( missing(units) && !missing(resolution) )
		units <- get_units_from_names(resolution, units)
	units <- switch(match.arg(units), ppm="relative", mz="absolute")
	if ( !is.na(resolution) )
		resolution <- switch(units,
			relative=1e-6 * resolution,
			absolute=resolution)
	if ( !is.na(tolerance) )
		tolerance <- switch(units,
			relative=1e-6 * tolerance,
			absolute=tolerance)
	ans <- callNextMethod(x, ref=ref, spectra=spectra, index=index,
		method=method, resolution=resolution, tolerance=tolerance, units=units)
	spectraData <- spectraData(ans)
	featureData <- as(featureData(ans), "MassDataFrame")
	new("MSImagingExperiment", spectraData=spectraData,
		featureData=featureData, elementMetadata=pixelData(x),
		experimentData=experimentData(x), centroided=centroided(x),
		metadata=metadata(x), processing=processingData(x))
})


# MSImagingArrays

setMethod("bin", "MSImagingArrays",
	function(x, ref,
		spectra = "intensity", index = "mz",
		method = c("sum", "mean", "max", "min",
			"linear", "cubic", "gaussian", "lanczos"),
		resolution = NA, tolerance = NA, units = c("ppm", "mz"),
		mass.range = NULL, ...)
{
	if ( !is.null(mass.range) ) {
		if ( is.na(resolution) )
			.Error("setting 'mass.range' requires setting 'resolution'")
		ref <- mass.range
	}
	if ( !missing(ref) ) {
		if ( is(ref, "MSImagingExperiment") || is(ref, "MassDataFrame") )
			ref <- mz(ref)
	}
	if ( missing(units) && !missing(resolution) )
		units <- get_units_from_names(resolution, units)
	units <- switch(match.arg(units), ppm="relative", mz="absolute")
	if ( !is.na(resolution) )
		resolution <- switch(units,
			relative=1e-6 * resolution,
			absolute=resolution)
	if ( !is.na(tolerance) )
		tolerance <- switch(units,
			relative=1e-6 * tolerance,
			absolute=tolerance)
	ans <- callNextMethod(x, ref=ref, spectra=spectra, index=index,
		method=method, resolution=resolution, tolerance=tolerance, units=units)
	spectraData <- spectraData(ans)
	featureData <- as(featureData(ans), "MassDataFrame")
	new("MSImagingExperiment", spectraData=spectraData,
		featureData=featureData, elementMetadata=pixelData(x),
		experimentData=experimentData(x), centroided=centroided(x),
		metadata=metadata(x), processing=processingData(x))
})


# SpectralImagingExperiment

setMethod("bin", "SpectralImagingExperiment",
	function(x, ref,
		spectra = "intensity", index = NULL,
		method = c("sum", "mean", "max", "min",
			"linear", "cubic", "gaussian", "lanczos"),
		resolution = NA, tolerance = NA, units = c("relative", "absolute"),
		verbose = getCardinalVerbose(), ...)
{
	method <- match.arg(method)
	if ( length(index) > 1L )
		.Error("more than 1 'index' array not allowed")
	if ( missing(units) && !missing(resolution) )
		units <- get_units_from_names(resolution, units)
	units <- match.arg(units)
	snm <- spectra
	inm <- index
	spectra <- spectra(x, snm)
	if ( is.null(inm) ) {
		inm <- "index"
		domain <- seq_len(nrow(x))
	} else {
		domain <- featureData(x)[[inm]]
		if ( is.null(domain) )
			.Error("index ", sQuote(inm), " not found")
	}
	if ( is.sparse(spectra) ) {
		index <- atomindex(spectra)
		spectra <- atomdata(spectra)
	} else {
		if ( is.matter(spectra(x)) ) {
			spectra <- as(spectra, "matter_list")
		} else {
			spectra <- apply(spectra, 2L, identity, simplify=FALSE)
		}
		index <- rep.int(list(domain), ncol(x))
	}
	res.ref <- switch(units, relative="x", absolute="abs")
	if ( is.na(resolution) ) {
		if ( missing(ref) || is.null(ref) ) {
			res <- estres(domain, ref=res.ref)
		} else {
			res <- estres(ref, ref=res.ref)
		}
	} else {
		res <- setNames(resolution, units)
		if ( !missing(ref) && !is.null(ref) )
			ref <- switch(units,
				relative=seq_rel(min(ref), max(ref), by=res),
				absolute=seq.default(min(ref), max(ref), by=res))
	}
	if ( missing(ref) || is.null(ref) ) {
		from <- floor(min(domain))
		to <- ceiling(max(domain))
		ref <- switch(units,
			relative=seq_rel(from, to, by=res),
			absolute=seq.default(from, to, by=res))
	}
	if ( is.na(tolerance) ) {
		if ( method %in% c("sum", "mean", "max", "min") ) {
			tol <- 0.5 * res
		} else {
			tol <- 2 * res
		}
		from <- round(min(ref), digits=4L)
		to <- round(max(ref), digits=4L)
		.Log("binning ", snm, " from ", inm, " ", from, " to ", to,
			" with ", units, " resolution ", round(res, digits=6L),
			message=verbose)
	} else {
		label <- if (length(ref) != 1L) "references" else "reference"
		.Log("binning ", snm, " to ", length(ref), " ", inm, " ", label,
			" with ", units, " tolerance ", round(tolerance, digits=6L),
			message=verbose)
		tol <- setNames(tolerance, units)
	}
	spectra <- sparse_mat(index=index,
		data=spectra, domain=ref,
		nrow=length(ref), ncol=length(x),
		tolerance=tol, sampler=method)
	spectraData <- SpectraArrays(setNames(list(spectra), snm))
	featureData <- DataFrame(setNames(list(ref), inm))
	new("SpectralImagingExperiment", spectraData=spectraData,
		featureData=featureData, elementMetadata=pixelData(x),
		metadata=metadata(x), processing=processingData(x))
})


# SpectralImagingArrays

setMethod("bin", "SpectralImagingArrays",
	function(x, ref,
		spectra = "intensity", index = NULL,
		method = c("sum", "mean", "max", "min",
			"linear", "cubic", "gaussian", "lanczos"),
		resolution = NA, tolerance = NA, units = c("relative", "absolute"),
		verbose = getCardinalVerbose(), ...)
{
	method <- match.arg(method)
	if ( length(index) > 1L )
		.Error("more than 1 'index' array not allowed")
	if ( missing(units) && !missing(resolution) )
		units <- get_units_from_names(resolution, units)
	units <- match.arg(units)
	snm <- spectra
	inm <- index
	spectra <- spectra(x, snm)
	if ( is.null(inm) ) {
		inm <- "index"
		index <- lapply(lengths(spectra), seq_len)
	} else {
		index <- spectra(x, inm)
		if ( is.null(index) )
			.Error("index ", sQuote(inm), " not found")
	}
	res.ref <- switch(units, relative="x", absolute="abs")
	if ( is.na(resolution) ) {
		if ( missing(ref) || is.null(ref) ) {
			res <- estres(index[[1L]], ref=res.ref)
		} else {
			res <- estres(ref, ref=res.ref)
		}
	} else {
		res <- setNames(resolution, units)
		if ( !missing(ref) && !is.null(ref) )
			ref <- switch(units,
				relative=seq_rel(min(ref), max(ref), by=res),
				absolute=seq.default(min(ref), max(ref), by=res))
	}
	if ( missing(ref) || is.null(ref) ) {
		from <- floor(min(index[[1L]]))
		to <- ceiling(max(index[[1L]]))
		ref <- switch(units,
			relative=seq_rel(from, to, by=res),
			absolute=seq.default(from, to, by=res))
	}
	if ( is.na(tolerance) ) {
		if ( method %in% c("sum", "mean", "max", "min") ) {
			tol <- 0.5 * res
		} else {
			tol <- 2 * res
		}
		from <- round(min(ref), digits=4L)
		to <- round(max(ref), digits=4L)
		.Log("binning ", snm, " from ", inm, " ", from, " to ", to,
			" with ", units, " resolution ", round(res, digits=6L),
			message=verbose)
	} else {
		tol <- setNames(tolerance, units)
		label <- if (length(ref) != 1L) "references" else "reference"
		.Log("binning ", snm, " to ", length(ref), " ", inm, " ", label,
			" with ", units, " tolerance ", round(tolerance, digits=6L),
			message=verbose)
	}
	spectra <- sparse_mat(index=index,
		data=spectra, domain=ref,
		nrow=length(ref), ncol=length(x),
		tolerance=tol, sampler=method)
	spectraData <- SpectraArrays(setNames(list(spectra), snm))
	featureData <- DataFrame(setNames(list(ref), inm))
	new("SpectralImagingExperiment", spectraData=spectraData,
		featureData=featureData, elementMetadata=pixelData(x),
		metadata=metadata(x), processing=processingData(x))
})

