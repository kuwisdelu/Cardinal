
#### Spectral binning ####
## --------------------------

# MSImagingExperiment

setMethod("bin", "MSImagingExperiment",
	function(x, ref,
		spectra = "intensity", index = "mz",
		method = c("sum", "mean", "max", "min",
			"linear", "cubic", "gaussian", "lanczos"),
		resolution = NA, units = c("ppm", "mz"), ...)
{
	if ( !missing(ref) ) {
		if ( is(ref, "MSImagingExperiment") || is(ref, "MassDataFrame") )
			ref <- mz(ref)
	}
	if ( missing(units) && !missing(tolerance) )
		units <- get_units_from_tolerance(tolerance, units)
	units <- switch(match.arg(units), ppm="relative", mz="absolute")
	if ( !is.na(resolution) )
		resolution <- switch(units,
			relative=1e-6 * resolution,
			absolute=resolution)
	ans <- callNextMethod(x, ref=ref, spectra=spectra, index=index,
		method=method, resolution=resolution, units=units)
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
		resolution = NA, units = c("ppm", "mz"), ...)
{
	if ( !missing(ref) ) {
		if ( is(ref, "MSImagingExperiment") || is(ref, "MassDataFrame") )
			ref <- mz(ref)
	}
	if ( missing(units) && !missing(tolerance) )
		units <- get_units_from_tolerance(tolerance, units)
	units <- switch(match.arg(units), ppm="relative", mz="absolute")
	if ( !is.na(resolution) )
		resolution <- switch(units,
			relative=1e-6 * resolution,
			absolute=resolution)
	ans <- callNextMethod(x, ref=ref, spectra=spectra, index=index,
		method=method, resolution=resolution, units=units)
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
		resolution = NA, units = c("relative", "absolute"),
		verbose = getCardinalVerbose(), ...)
{
	method <- match.arg(method)
	if ( missing(units) && !missing(tolerance) )
		units <- get_units_from_tolerance(tolerance, units)
	units <- match.arg(units)
	xnm <- spectra
	tnm <- index
	spectra <- spectra(x, xnm)
	if ( is.null(tnm) ) {
		tnm <- "index"
		domain <- seq_len(nrow(x))
	} else {
		domain <- featureData(x)[[tnm]]
		if ( is.null(domain) )
			stop("index ", sQuote(tnm), " not found")
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
	}
	if ( missing(ref) || is.null(ref) ) {
		from <- floor(min(domain))
		to <- ceiling(max(domain))
		ref <- switch(units,
			relative=seq_rel(from, to, by=res),
			absolute=seq.default(from, to, by=res))
	}
	from <- round(min(ref), digits=4L)
	to <- round(max(ref), digits=4L)
	if ( verbose )
		message("binned ", xnm, " from ", tnm, " ", from, " to ", to,
			" with ", units, " resolution ", round(res, digits=6L))
	if ( method %in% c("sum", "mean", "max", "min") ) {
		tol <- 0.5 * res
	} else {
		tol <- 2 * res
	}
	spectra <- sparse_mat(index=index,
		data=spectra, domain=ref,
		nrow=length(ref), ncol=length(x),
		tolerance=tol, sampler=method)
	spectraData <- SpectraArrays(setNames(list(spectra), xnm))
	featureData <- DataFrame(setNames(list(ref), tnm))
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
		resolution = NA, units = c("relative", "absolute"),
		verbose = getCardinalVerbose(), ...)
{
	method <- match.arg(method)
	if ( missing(units) && !missing(tolerance) )
		units <- get_units_from_tolerance(tolerance, units)
	units <- match.arg(units)
	xnm <- spectra
	tnm <- index
	spectra <- spectra(x, xnm)
	if ( is.null(tnm) ) {
		tnm <- "index"
		index <- lapply(lengths(spectra), seq_len)
	} else {
		index <- spectra(x, tnm)
		if ( is.null(index) )
			stop("index ", sQuote(tnm), " not found")
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
	}
	if ( missing(ref) || is.null(ref) ) {
		from <- floor(min(index[[1L]]))
		to <- ceiling(max(index[[1L]]))
		ref <- switch(units,
			relative=seq_rel(from, to, by=res),
			absolute=seq.default(from, to, by=res))
	}
	from <- round(min(ref), digits=4L)
	to <- round(max(ref), digits=4L)
	if ( verbose )
		message("binned ", xnm, " from ", tnm, " ", from, " to ", to,
			" with ", units, " resolution ", round(res, digits=6L))
	if ( method %in% c("sum", "mean", "max", "min") ) {
		tol <- 0.5 * res
	} else {
		tol <- 2 * res
	}
	spectra <- sparse_mat(index=index,
		data=spectra, domain=ref,
		nrow=length(ref), ncol=length(x),
		tolerance=tol, sampler=method)
	spectraData <- SpectraArrays(setNames(list(spectra), xnm))
	featureData <- DataFrame(setNames(list(ref), tnm))
	new("SpectralImagingExperiment", spectraData=spectraData,
		featureData=featureData, elementMetadata=pixelData(x),
		metadata=metadata(x), processing=processingData(x))
})

