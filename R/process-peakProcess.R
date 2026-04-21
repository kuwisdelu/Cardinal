
#### Peak processing ####
## ----------------------

setMethod("peakProcess", "MSImagingArrays",
	function(object, ref,
		method = c("diff", "sd", "mad", "quantile", "filter", "cwt"),
		SNR = 2, type = c("height", "area"),
		tolerance = NA, units = c("ppm", "mz"),
		sampleSize = NA, filterFreq = TRUE, outfile = NULL,
		f = processingChunkFactor(object),
		verbose = getCardinalVerbose(),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( missing(units) && !missing(tolerance) )
		units <- get_units_from_names(tolerance, units)
	units <- match.arg(units)
	if ( !is.na(sampleSize) ||
		(!isCentroided(object) && !missing(ref) && !is.null(ref)) )
	{
		# need to do peak picking
		if ( missing(ref) || is.null(ref) )
		{
			# create reference peaks from sample spectra
			if ( sampleSize < 1 ) {
				# sample size is a proportion
				n <- ceiling(sampleSize * length(object))
				perc <- 100 * sampleSize
			} else if ( sampleSize > 0 ) {
				# sample size is a count
				n <- min(sampleSize, length(object))
				perc <- round(100 * n / length(object))
			} else {
				.Error("'sampleSize' must be positive")
			}
			label <- if (n != 1L) "spectra" else "spectrum"
			.Log("processing peaks for ", n, " ", label, " ",
				"(~", perc, "% of data)",
				message=verbose)
			i <- seq.default(1L, length(object), length.out=n)
			ref <- peakProcess(object[i], f=droplevels(f[i]),
				method=method, SNR=SNR, type=type,
				tolerance=tolerance, units=units, filterFreq=filterFreq,
				verbose=verbose, BPPARAM=BPPARAM, ...)
			domain <- mz(ref)
		} else {
			if ( is(ref, "MSImagingExperiment") || is(ref, "MassDataFrame") )
				ref <- mz(ref)
			domain <- as.numeric(ref)
		}
		# extract the peaks based on reference
		.Log("extracting reference peaks from all spectra",
			message=verbose)
		object <- peakPick(object, ref=ref,
			tolerance=tolerance, units=units, type=type)
		object <- applyProcessing(object,
			f=f, verbose=verbose, BPPARAM=BPPARAM, ...)
		object <- bin(object, ref=ref,
			tolerance=tolerance, units=units, verbose=FALSE)
		if ( is(ref, "MSImagingExperiment") )
			featureData(object) <- featureData(ref)
	} else {
		# check for peak picking
		if ( isCentroided(object) ) {
			if ( length(processingData(object)) == 0L &&
				!is.sparse(spectra(object, spectra)) &&
				!is(object, "MSImagingArrays") )
			{
				.Log("peaks are already processed",
					message=verbose)
				return(object)
			} else {
				.Log("peaks are already detected",
					message=verbose)
			}
		} else {
			# pick peaks on all spectra
			.Log("queueing peak picking",
				message=verbose)
			object <- peakPick(object,
				method=method, SNR=SNR, type=type)
		}
		# align peaks across all spectra
		.Log("applying peak alignment",
			message=verbose)
		object <- peakAlign(object, ref=ref,
			tolerance=tolerance, units=units,
			f=f, verbose=verbose, BPPARAM=BPPARAM, ...)
		# filter peaks
		if ( !is.null(featureData(object)[["count"]]) &&
			(isTRUE(filterFreq) || filterFreq > 0) )
		{
			if ( is.numeric(filterFreq) ) {
				if ( filterFreq < 1 ) {
					# filterFeq is a proportion
					n <- ceiling(filterFreq * length(object))
				} else if ( filterFreq > 0) {
					# filterFeq is a count
					n <- as.integer(filterFreq)
				} else {
					.Error("'filterFreq' must be positive")
				}
			} else {
				# remove singleton peaks
				n <- 1L
			}
			label <- if (n / length(object) < 0.01) "<" else "~"
			.Log("filtering to keep only peaks with counts > ", n, " ",
				"(", label, round(100 * n / length(object), digits=2L),
				"% of considered spectra)",
				message=verbose)
			object <- object[featureData(object)[["count"]] > n,]
		}
	}
	# return object
	.Log("processed to ", nrow(object), " peaks",
		message=verbose)
	if ( validObject(object) )
		object
})
