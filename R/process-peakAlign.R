
#### Peak alignment ####
## ---------------------

setMethod("peakAlign", "MSImagingArrays",
	function(object, ref,
		binfun = "min", binratio = 2,
		tolerance = NA, units = c("ppm", "mz"),
		f = processingChunkFactor(object),
		REDUCE, init, reduce.in.order = TRUE,
		verbose = getCardinalVerbose(),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( length(processingData(object)) > 0L ) {
		.Log("applying queued processing",
			message=verbose)
		object <- applyProcessing(object, f=f, ...,
			REDUCE=REDUCE, init=init, reduce.in.order=reduce.in.order,
			verbose=verbose, BPPARAM=BPPARAM)
	}
	if ( missing(units) && !missing(tolerance) )
		units <- get_units_from_names(tolerance, units)
	units <- match.arg(units)
	if ( missing(ref) || is.null(ref) ) {
		# need to create reference m/z-values
		width <- match.arg(binfun, c("median", "min", "max", "mean"))
		.Log("summarizing m/z range for alignment",
			message=verbose)
		.Log("using bin function ", sQuote(width),
			" to summarize peak gaps across spectra",
			message=verbose)
		mzref <- estimateReferenceMz(object, width=width, units=units,
			f=f, verbose=verbose, BPPARAM=BPPARAM)
		# resolve peak bins and tolerance
		if ( is.na(tolerance) ) {
			# we don't have tolerance or peak bins
			# estimate tolerance as (binratio x min peak-to-peak gap)
			# NOTE: refactor later to avoid all these unit conversions
			resolution <- estres(mzref, ref=switch(units, ppm="x", mz="abs"))
			resolution <- resolution * switch(units, ppm=1e6, mz=1)
			resolution <- switch(units,
				ppm=round(2 * resolution, digits=6L) * 0.5,
				mz=round(resolution, digits=4L))
			tolerance <- binratio * resolution
			.Log("using bin ratio ", binratio,
				" to compute tolerance from estimated peak bins",
				message=verbose)
			# create peak bins from estimated reference m/z-values
			ref <- mzref
		} else {
			# we have tolerance but not peak bins
			# set peak bins to (tolerance / binratio)
			resolution <- tolerance / binratio
			.Log("using bin ratio ", binratio,
				" to create peak bins (per tolerance half-window)",
				message=verbose)
			# create peak bins from tolerance and m/z range
			# NOTE: refactor later to avoid all these unit conversions
			ref <- switch(units,
				ppm=seq_rel(min(mzref), max(mzref), by=1e-6 * resolution),
				mz=seq(min(mzref), max(mzref), by=resolution))
		}
		.Log("using peak bins with resolution ", resolution, " ", units,
			message=verbose)
		.Log("using peak matching tolerance ", tolerance, " ", units,
			message=verbose)
	} else {
		if ( is(ref, "MSImagingExperiment") || is(ref, "MassDataFrame") )
			ref <- mz(ref)
		if ( is.na(tolerance) )
			.Error("'tolerance' must be specified when 'ref' is provided")
	}
	# format tolerance and tolerance type for pkg:matter functions
	# NOTE: refactor later to avoid all these unit conversions
	tol <- unname(tolerance) * switch(units, ppm=1e-6, mz=1)
	tol <- setNames(tol, switch(units, ppm="relative", mz="absolute"))
	tol.ref <- switch(units, ppm="x", mz="abs")
	# apply peak binning
	peaks <- .chunkapply_SpectralImagingArrays(object, ...,
		CHUNKFUN=.peakAlign_MSImagingArrays,
		ref=ref, tol=tol, tol.ref=tol.ref,
		REDUCE=matter::stat_c, reduce.in.order=reduce.in.order,
		f=f, verbose=verbose, BPPARAM=BPPARAM)
	.Log("merging peak bins with centroid differences",
		" <= ", tolerance, " ", units,
		message=verbose)
	# merge overlapping peaks
	peaks <- peaks[!is.na(peaks)]
	peaks <- mergepeaks(peaks, tol=tol, tol.ref=tol.ref)
	n <- nobs(peaks)
	mzout <- structure(as.vector(peaks), n=n)
	.Log("aligned to ", length(peaks),
		" reference peaks with tolerance",
		" of ", tolerance, " ", units,
		message=verbose)
	# build MSImagingExperiment
	intensity <- sparse_mat(
		index=mz(object), data=intensity(object), domain=mzout,
		tolerance=tol, sampler="none")
	spectraData <- SpectraArrays(list(intensity=intensity))
	featureData <- MassDataFrame(mz=mzout,
		count=n, freq=n / length(object))
	new("MSImagingExperiment",
		spectraData=spectraData,
		featureData=featureData,
		elementMetadata=pixelData(object),
		experimentData=experimentData(object),
		centroided=TRUE,
		metadata=metadata(object))
})

.peakAlign_MSImagingArrays <- function(x, ref, tol, tol.ref)
{
	matter::binpeaks(mz(x), domain=ref,
		tol=tol, tol.ref=tol.ref, merge=FALSE, na.drop=FALSE)
}
