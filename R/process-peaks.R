
#### Peak processing ####
## ----------------------

setMethod("peakPick", "MSImagingExperiment",
	function(object, ref,
		method = c("diff", "sd", "mad", "quantile", "filter", "cwt"),
		tolerance = NA, units = c("ppm", "mz"),
		SNR = 2, type = c("height", "area"), ...)
{
	if ( isCentroided(object) ) {
		stop("object is already centroided")
	} else {
		centroided(object) <- TRUE
	}
	units <- switch(match.arg(units), ppm="relative", mz="absolute")
	if ( !is.na(tolerance) )
		tolerance <- switch(units,
			relative=1e-6 * tolerance,
			absolute=tolerance)
	callNextMethod(object, ref=ref, method=method, SNR=SNR,
		type=type, tolerance=tolerance, units=units, ...)
})


#### Peak picking ####
## -------------------

setMethod("peakPick", "MSImagingExperiment",
	function(object, ref,
		method = c("diff", "sd", "mad", "quantile", "filter", "cwt"),
		tolerance = NA, units = c("ppm", "mz"),
		SNR = 2, type = c("height", "area"), ...)
{
	if ( isCentroided(object) ) {
		stop("object is already centroided")
	} else {
		centroided(object) <- TRUE
	}
	units <- switch(match.arg(units), ppm="relative", mz="absolute")
	if ( !is.na(tolerance) )
		tolerance <- switch(units,
			relative=1e-6 * tolerance,
			absolute=tolerance)
	callNextMethod(object, ref=ref, method=method, SNR=SNR,
		type=type, tolerance=tolerance, units=units, ...)
})

setMethod("peakPick", "MSImagingArrays",
	function(object, ref,
		method = c("diff", "sd", "mad", "quantile", "filter", "cwt"),
		tolerance = NA, units = c("ppm", "mz"),
		SNR = 2, type = c("height", "area"), ...)
{
	if ( isCentroided(object) ) {
		stop("object is already centroided")
	} else {
		centroided(object) <- TRUE
	}
	units <- switch(match.arg(units), ppm="relative", mz="absolute")
	if ( !is.na(tolerance) )
		tolerance <- switch(units,
			relative=1e-6 * tolerance,
			absolute=tolerance)
	callNextMethod(object, ref=ref, method=method, SNR=SNR,
		type=type, tolerance=tolerance, units=units, ...)
})

setMethod("peakPick", "SpectralImagingData",
	function(object, ref,
		method = c("diff", "sd", "mad", "quantile", "filter", "cwt"),
		tolerance = NA, units = c("relative", "absolute"),
		SNR = 2, type = c("height", "area"), ...)
{
	method <- match.arg(method)
	type <- match.arg(type)
	if ( missing(ref) || is.null(ref) ) {
		if ( !is.na(tolerance) )
			warning("no 'ref' given so 'tolerance' will be ignored")
		if ( method == "cwt" ) {
			FUN <- .peakPick_cwt
		} else {
			FUN <- .peakPick
		}
		addProcessing(object, FUN, label=paste0(type, " peak picking"),
			method=method, SNR=SNR, type=type, ...)
	} else {
		units <- match.arg(units)
		if ( is.unsorted(ref) )
			ref <- sort(ref)
		tol.ref <- switch(units, relative="x", absolute="abs")
		if ( is.na(tolerance) ) {
			tol <- 0.5 * estres(ref, ref=tol.ref)
		} else {
			tol <- tolerance
		}
		FUN <- .peakPick_ref
		addProcessing(object, FUN, label=paste0(type, " peak picking"),
			ref=ref, tol=tol, tol.ref=tol.ref, type=type, ...)
	}
})

.peakPick <- function(x, t, method, ..., SNR = 2, type = "height")
{
	peaks <- matter::findpeaks(x, noise=method, snr=SNR, relheight=0, ...)
	if ( type == "height" ) {
		cbind(t[peaks], x[peaks])
	} else if ( type == "area" ) {
		cbind(t[peaks], matter::peakareas(x, peaks, domain=t))
	} else {
		stop("invalid peak type: ", sQuote(type))
	}
}

.peakPick_cwt <- function(x, t, method, ..., SNR = 2, type = "height")
{
	peaks <- matter::findpeaks_cwt(x, snr=SNR, ...)
	if ( type == "height" ) {
		cbind(t[peaks], x[peaks])
	} else if ( type == "area" ) {
		cbind(t[peaks], matter::peakareas(x, peaks, domain=t))
	} else {
		stop("invalid peak type: ", sQuote(type))
	}
}

.peakPick_ref <- function(x, t, ref, tol, tol.ref,..., type = "height")
{
	peaks <- matter::findpeaks(x, relheight=0, ...)
	hits <- bsearch(ref, t[peaks], tol=tol, tol.ref=tol.ref)
	nz <- !is.na(hits)
	values <- numeric(length(ref))
	if ( type == "height" ) {
		values[nz] <- matter::peakheights(x, peaks[hits[nz]])
	} else if ( type == "area" ) {
		values[nz] <- matter::peakareas(x, peaks[hits[nz]], domain=t)
	} else {
		stop("invalid peak type: ", sQuote(type))
	}
	cbind(ref, values)
}


#### Peak alignment ####
## ---------------------

setMethod("peakAlign", "MSImagingExperiment",
	function(object, ref,
		spectra = "intensity", index = "mz",
		tolerance = NA, units = c("ppm", "mz"),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	units <- switch(match.arg(units), ppm="relative", mz="absolute")
	if ( !is.na(tolerance) )
		tolerance <- switch(units,
			relative=1e-6 * tolerance,
			absolute=tolerance)
	ans <- callNextMethod(object, ref=ref, spectra=spectra, index=index,
		tolerance=tolerance, units=units, BPPARAM=BPPARAM)
	spectraData <- spectraData(ans)
	featureData <- as(featureData(ans), "MassDataFrame")
	new("MSImagingExperiment", spectraData=spectraData,
		featureData=featureData, elementMetadata=pixelData(object),
		experimentData=experimentData(object), centroided=TRUE,
		metadata=metadata(object), processing=list())
})

setMethod("peakAlign", "MSImagingArrays",
	function(object, ref,
		spectra = "intensity", index = "mz",
		tolerance = NA, units = c("ppm", "mz"),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	units <- switch(match.arg(units), ppm="relative", mz="absolute")
	if ( !is.na(tolerance) )
		tolerance <- switch(units,
			relative=1e-6 * tolerance,
			absolute=tolerance)
	ans <- callNextMethod(object, ref=ref, spectra=spectra, index=index,
		tolerance=tolerance, units=units, BPPARAM=BPPARAM)
	spectraData <- spectraData(ans)
	featureData <- as(featureData(ans), "MassDataFrame")
	new("MSImagingExperiment", spectraData=spectraData,
		featureData=featureData, elementMetadata=pixelData(object),
		experimentData=experimentData(object), centroided=TRUE,
		metadata=metadata(object), processing=list())
})

setMethod("peakAlign", "SpectralImagingExperiment",
	function(object, ref,
		spectra = "intensity", index = NULL,
		tolerance = NA, units = c("relative", "absolute"),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	units <- match.arg(units)
	xnm <- spectra
	tnm <- index
	spectra <- spectra(object, xnm)
	if ( is.null(tnm) ) {
		tnm <- "index"
		domain <- seq_len(nrow(object))
	} else {
		domain <- featureData(object)[[tnm]]
		if ( is.null(domain) )
			stop("index ", sQuote(tnm), " not found")
	}
	if ( is.sparse(spectra) ) {
		index <- atomindex(spectra)
		spectra <- atomdata(spectra)
	} else {
		stop("nothing to align for spectra ", sQuote(xnm), "; ",
			"has peakPick() been used?")
	}
	if ( getCardinalVerbose() )
		message("detected ~", round(mean(lengths(index)), digits=1L),
			" peaks per spectrum")
	.peakAlign(object, ref=ref, spectra=spectra, index=index,
		domain=domain, xname=xnm, tname=tnm,
		tolerance=tolerance, units=units,
		BPPARAM=BPPARAM)
})

setMethod("peakAlign", "SpectralImagingArrays",
	function(object, ref,
		spectra = "intensity", index = NULL,
		tolerance = NA, units = c("relative", "absolute"),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	units <- match.arg(units)
	xnm <- spectra
	tnm <- index
	spectra <- spectra(object, xnm)
	if ( is.null(tnm) ) {
		tnm <- "index"
		index <- lapply(lengths(spectra), seq_len)
	} else {
		index <- spectra(object, tnm)
		if ( is.null(index) )
			stop("index ", sQuote(tnm), " not found")
	}
	if ( getCardinalVerbose() )
		message("detected ~", round(mean(lengths(index)), digits=1L),
			" peaks per spectrum")
	if ( missing(ref) || is.null(ref) ) {
		if ( getCardinalVerbose() )
			message("generating reference peaks")
		domain <- estimateDomain(index, units=units, BPPARAM=BPPARAM)
	} else {
		domain <- ref
	}
	.peakAlign(object, ref=ref, spectra=spectra, index=index,
		domain=domain, xname=xnm, tname=tnm,
		tolerance=tolerance, units=units,
		BPPARAM=BPPARAM)
})

.peakAlign <- function(object, ref, spectra, index,
	domain, xname, tname, tolerance, units, BPPARAM)
{
	tol.ref <- switch(units, relative="x", absolute="abs")
	if ( is.na(tolerance) ) {
		tol <- 0.5 * estres(domain, ref=tol.ref)
		tol <- switch(units,
			relative=round(2 * tol, digits=6L) * 0.5,
			absolute=round(tol, digits=4L))
	} else {
		tol <- setNames(unname(tolerance), units)
	}
	if ( missing(ref) || is.null(ref) ) {
		if ( getCardinalVerbose() )
			message("binning peaks to reference")
		FUN <- function(x) {
			matter::binpeaks(x, domain=domain, tol=tol, tol.ref=tol.ref,
				merge=FALSE, na.drop=FALSE)
		}
		peaks <- chunk_lapply(index, FUN, simplify=matter::stat_c,
			nchunks=getCardinalNChunks(),
			verbose=getCardinalVerbose(),
			BPPARAM=BPPARAM)
		peaks <- peaks[!is.na(peaks)]
		peaks <- mergepeaks(peaks, tol=tol, tol.ref=tol.ref)
		n <- nobs(peaks)
		ref <- structure(as.vector(peaks), n=n)
	} else {
		n <- NULL
	}
	if ( getCardinalVerbose() )
		message("aligned to ", length(ref),
			" reference peaks with ", units,
				" tolerance ", tol)
	spectra <- sparse_mat(index=index,
		data=spectra, domain=ref,
		nrow=length(ref), ncol=length(object),
		tolerance=tol, sampler="none")
	spectraData <- SpectraArrays(setNames(list(spectra), xname))
	featureData <- DataFrame(setNames(list(ref), tname))
	if ( !is.null(n) ) {
		featureData[["count"]] <- n
		featureData[["freq"]] <- n / length(spectra)
	}
	new("SpectralImagingExperiment", spectraData=spectraData,
		featureData=featureData, elementMetadata=pixelData(object),
		metadata=metadata(object), processing=list())
}

