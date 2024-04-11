
#### Peak processing ####
## ----------------------

setMethod("peakProcess", "MSImagingExperiment_OR_Arrays",
	function(object, ref,
		spectra = "intensity", index = "mz",
		method = c("diff", "sd", "mad", "quantile", "filter", "cwt"),
		SNR = 2, type = c("height", "area"),
		tolerance = NA, units = c("ppm", "mz"),
		sampleSize = Inf, filterFreq = TRUE, outfile = NULL,
		nchunks = getCardinalNChunks(),
		verbose = getCardinalVerbose(),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( (!missing(ref) && !is.null(ref)) || is.finite(sampleSize) )
	{
		# extract peaks based on a reference
		if ( missing(ref) || is.null(ref) )
		{
			# create reference peaks from sample spectra
			if ( sampleSize <= 1 ) {
				# sample size is a proportion
				n <- ceiling(sampleSize * length(object))
				perc <- 100 * sampleSize
			} else if ( sampleSize > 0 ) {
				# sample size is a count
				n <- min(sampleSize, length(object))
				perc <- round(100 * n / length(object))
			} else {
				stop("'sampleSize' must be positive")
			}
			if ( verbose ) {
				label <- if (n != 1L) "spectra" else "spectrum"
				message("processing peaks for ", n, " ", label, " ",
					"(~", perc, "% of data)")
			}
			i <- seq.default(1L, length(object), length.out=n)
			ref <- peakProcess(object[i],
				spectra=spectra, index=index,
				method=method, SNR=SNR, type=type,
				tolerance=tolerance, units=units, filterFreq=filterFreq,
				nchunks=nchunks, verbose=verbose,
				BPPARAM=BPPARAM, ...)
			domain <- mz(ref)
		} else {
			# check if peaks are already processed
			if ( is(ref, "MSImagingExperiment") || is(ref, "MassDataFrame") )
				ref <- mz(ref)
			if ( isTRUE(all.equal(mz(object), ref)) ) {
				if ( verbose )
					message("peaks are already processed")
				return(object)
			}
			domain <- as.numeric(ref)
		}
		# extract the peaks
		if ( verbose )
			message("extracting reference peaks from all spectra")
		object <- peakPick(object, ref=ref,
			tolerance=tolerance, units=units, type=type)
		object <- process(object,
			spectra=spectra, index=index,
			domain=domain, outfile=outfile,
			nchunks=nchunks, verbose=verbose,
			BPPARAM=BPPARAM, ...)
		if ( is(ref, "MSImagingExperiment") )
			featureData(object) <- featureData(ref)
	} else {
		if ( isCentroided(object) ) {
			if ( length(processingData(object)) == 0L &&
				!is.sparse(spectra(object, spectra)) )
			{
				if ( verbose )
					message("peaks are already processed")
				return(object)
			} else {
				if ( verbose )
					message("peaks are already detected")
			}
		} else {
			# pick peaks on all spectra
			object <- peakPick(object,
				method=method, SNR=SNR, type=type)
		}
		# align peaks across all spectra
		object <- peakAlign(object,
			spectra=spectra, index=index, outfile=outfile,
			tolerance=tolerance, units=units,
			nchunks=nchunks, verbose=verbose,
			BPPARAM=BPPARAM, ...)
		# filter peaks
		if ( isTRUE(filterFreq) || filterFreq > 0 ) {
			if ( is.numeric(filterFreq) ) {
				if ( filterFreq < 1 ) {
					# filterFeq is a proportion
					n <- ceiling(filterFreq * length(object))
				} else if ( filterFreq > 0) {
					# filterFeq is a count
					n <- as.integer(filterFreq)
				} else {
					stop("'filterFreq' must be positive")
				}
			} else {
				# remove singleton peaks
				n <- 1L
			}
			if ( verbose )
				message("filtering to keep only peaks with counts > ", n, " ",
					"(", ceiling(n / length(object)), "% of considered spectra)")
			object <- object[featureData(object)[["count"]] > n,]
		}
	}
	# return object
	if ( verbose )
		message("processed to ", nrow(object), " peaks")
	if ( validObject(object) )
		object
})


#### Peak alignment ####
## ---------------------

setMethod("peakAlign", "MSImagingExperiment",
	function(object, ref,
		spectra = "intensity", index = "mz",
		tolerance = NA, units = c("ppm", "mz"), ...)
{
	if ( !missing(ref) ) {
		if ( is(ref, "MSImagingExperiment") || is(ref, "MassDataFrame") )
			ref <- mz(ref)
	}
	if ( missing(units) && !missing(tolerance) )
		units <- get_units_from_tolerance(tolerance, units)
	units <- switch(match.arg(units), ppm="relative", mz="absolute")
	if ( !is.na(tolerance) )
		tolerance <- switch(units,
			relative=1e-6 * tolerance,
			absolute=tolerance)
	ans <- callNextMethod(object, ref=ref, spectra=spectra, index=index,
		tolerance=tolerance, units=units, ...)
	spectraData <- spectraData(ans)
	featureData <- as(featureData(ans), "MassDataFrame")
	new("MSImagingExperiment", spectraData=spectraData,
		featureData=featureData, elementMetadata=pixelData(ans),
		experimentData=experimentData(object), centroided=TRUE,
		metadata=metadata(ans), processing=list())
})

setMethod("peakAlign", "MSImagingArrays",
	function(object, ref,
		spectra = "intensity", index = "mz",
		tolerance = NA, units = c("ppm", "mz"), ...)
{
	if ( !missing(ref) ) {
		if ( is(ref, "MSImagingExperiment") || is(ref, "MassDataFrame") )
			ref <- mz(ref)
	}
	if ( missing(units) && !missing(tolerance) )
		units <- get_units_from_tolerance(tolerance, units)
	units <- switch(match.arg(units), ppm="relative", mz="absolute")
	if ( !is.na(tolerance) )
		tolerance <- switch(units,
			relative=1e-6 * tolerance,
			absolute=tolerance)
	ans <- callNextMethod(object, ref=ref, spectra=spectra, index=index,
		tolerance=tolerance, units=units, ...)
	spectraData <- spectraData(ans)
	featureData <- as(featureData(ans), "MassDataFrame")
	new("MSImagingExperiment", spectraData=spectraData,
		featureData=featureData, elementMetadata=pixelData(ans),
		experimentData=experimentData(object), centroided=TRUE,
		metadata=metadata(ans), processing=list())
})

setMethod("peakAlign", "SpectralImagingExperiment",
	function(object, ref,
		spectra = "intensity", index = NULL,
		tolerance = NA, units = c("relative", "absolute"),
		nchunks = getCardinalNChunks(),
		verbose = getCardinalVerbose(),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( length(processingData(object)) > 0L )
		object <- process(object, spectra=spectra, index=index,
			nchunks=nchunks, verbose=verbose,
			BPPARAM=BPPARAM, ...)
	if ( missing(units) && !missing(tolerance) )
		units <- get_units_from_tolerance(tolerance, units)
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
	if ( !missing(ref) && !is.null(ref) ) {
		if ( isTRUE(all.equal(domain, ref)) ) {
			if ( verbose )
				message("peaks are already aligned")
			return(object)
		}
	}
	if ( is.sparse(spectra) ) {
		index <- atomindex(spectra)
		spectra <- atomdata(spectra)
	} else {
		stop("nothing to align for spectra ", sQuote(xnm), "; ",
			"has peakPick() been used?")
	}
	if ( verbose )
		message("detected ~", round(mean(lengths(index)), digits=1L),
			" peaks per spectrum")
	.peakAlign(object, ref=ref, spectra=spectra, index=index,
		domain=domain, xname=xnm, tname=tnm,
		tolerance=tolerance, units=units,
		nchunks=nchunks, verbose=verbose,
		BPPARAM=BPPARAM)
})

setMethod("peakAlign", "SpectralImagingArrays",
	function(object, ref,
		spectra = "intensity", index = NULL,
		tolerance = NA, units = c("relative", "absolute"),
		nchunks = getCardinalNChunks(),
		verbose = getCardinalVerbose(),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( length(processingData(object)) > 0L )
		object <- process(object, spectra=spectra, index=index,
			nchunks=nchunks, verbose=verbose,
			BPPARAM=BPPARAM, ...)
	if ( missing(units) && !missing(tolerance) )
		units <- get_units_from_tolerance(tolerance, units)
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
	if ( verbose )
		message("detected ~", round(mean(lengths(index)), digits=1L),
			" peaks per spectrum")
	if ( missing(ref) || is.null(ref) ) {
		if ( verbose )
			message("generating reference peaks")
		domain <- estimateDomain(index, units=units,
			nchunks=nchunks, verbose=verbose, BPPARAM=BPPARAM)
	} else {
		domain <- ref
	}
	.peakAlign(object, ref=ref, spectra=spectra, index=index,
		domain=domain, xname=xnm, tname=tnm,
		tolerance=tolerance, units=units,
		nchunks=nchunks, verbose=verbose,
		BPPARAM=BPPARAM)
})

.peakAlign <- function(object, ref, spectra, index,
	domain, xname, tname, tolerance, units,
	nchunks, verbose, BPPARAM)
{
	tol.ref <- switch(units, relative="x", absolute="abs")
	if ( is.na(tolerance) ) {
		# estimate tolerance as 2x resolution of domain
		tol <- 2 * estres(domain, ref=tol.ref)
		tol <- switch(units,
			relative=round(2 * tol, digits=6L) * 0.5,
			absolute=round(tol, digits=4L))
	} else {
		# re-estimate domain with 1/2 resolution as tolerance
		tol <- setNames(unname(tolerance), units)
		res <- 0.5 * tol
		domain <- switch(units,
			relative=seq_rel(min(domain), max(domain), by=res),
			absolute=seq.default(min(domain), max(domain), by=res))
	}
	if ( missing(ref) || is.null(ref) ) {
		if ( verbose )
			message("binning peaks to create shared reference")
		FUN <- function(x) {
			matter::binpeaks(x, domain=domain, tol=tol, tol.ref=tol.ref,
				merge=FALSE, na.drop=FALSE)
		}
		peaks <- chunk_lapply(index, FUN, simplify=matter::stat_c,
			nchunks=nchunks, verbose=verbose,
			BPPARAM=BPPARAM)
		peaks <- peaks[!is.na(peaks)]
		peaks <- mergepeaks(peaks, tol=tol, tol.ref=tol.ref)
		n <- nobs(peaks)
		ref <- structure(as.vector(peaks), n=n)
	} else {
		n <- NULL
	}
	if ( verbose ) {
		ppm <- switch(units,
			relative=paste0("(", 1e6 * tol, " ppm)"),
			absolute="")
		message("aligned to ", length(ref),
			" reference peaks with ", units,
				" tolerance ", tol, " ", ppm)
	}
	spectra <- sparse_mat(index=index,
		data=spectra, domain=ref,
		nrow=length(ref), ncol=length(object),
		tolerance=tol, sampler="none")
	spectraData <- SpectraArrays(setNames(list(spectra), xname))
	featureData <- DataFrame(setNames(list(ref), tname))
	if ( !is.null(n) ) {
		featureData[["count"]] <- n
		featureData[["freq"]] <- n / length(object)
	}
	new("SpectralImagingExperiment", spectraData=spectraData,
		featureData=featureData, elementMetadata=pixelData(object),
		metadata=metadata(object), processing=list())
}


#### Peak picking ####
## -------------------

setMethod("peakPick", "MSImagingExperiment",
	function(object, ref,
		method = c("diff", "sd", "mad", "quantile", "filter", "cwt"),
		SNR = 2, type = c("height", "area"),
		tolerance = NA, units = c("ppm", "mz"), ...)
{
	if ( !missing(ref) ) {
		if ( is(ref, "MSImagingExperiment") || is(ref, "MassDataFrame") )
			ref <- mz(ref)
	}
	if ( isCentroided(object) ) {
		stop("object is already centroided")
	} else {
		centroided(object) <- TRUE
	}
	if ( missing(units) && !missing(tolerance) )
		units <- get_units_from_tolerance(tolerance, units)
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
		SNR = 2, type = c("height", "area"),
		tolerance = NA, units = c("ppm", "mz"), ...)
{
	if ( !missing(ref) ) {
		if ( is(ref, "MSImagingExperiment") || is(ref, "MassDataFrame") )
			ref <- mz(ref)
	}
	if ( isCentroided(object) ) {
		stop("object is already centroided")
	} else {
		centroided(object) <- TRUE
	}
	if ( missing(units) && !missing(tolerance) )
		units <- get_units_from_tolerance(tolerance, units)
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
		SNR = 2, type = c("height", "area"),
		tolerance = NA, units = c("relative", "absolute"), ...)
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
		if ( missing(units) && !missing(tolerance) )
			units <- get_units_from_tolerance(tolerance, units)
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


#### Estimate reference peaks ####
## ------------------------------

estimateReferencePeaks <- function(object, SNR = 2,
	method = c("diff", "sd", "mad", "quantile", "filter", "cwt"),
	nchunks = getCardinalNChunks(),
	verbose = getCardinalVerbose(),
	BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( length(processingData(object)) > 0L )
		warning("processing steps will be ignored by estimateReferencePeaks()")
	method <- match.arg(method)
	object <- summarizeFeatures(object, stat="mean",
		nchunks=nchunks, verbose=verbose,
		BPPARAM=BPPARAM)
	featureData <- featureData(object)
	peaks <- findpeaks(featureData[["mean"]], noise=method, snr=SNR, ...)
	featureData[peaks,,drop=FALSE]
}


