
# #### Peak alignment ####
# ## ---------------------

# setMethod("peakAlign", "MSImagingArrays",
# 	function(object, ref,
# 		binfun = "min", binratio = 2,
# 		tolerance = NA, units = c("ppm", "mz"),
# 		f = processingChunkFactor(object),
# 		verbose = getCardinalVerbose(),
# 		BPPARAM = getCardinalBPPARAM(), ...)
# {
# 	if ( !missing(ref) ) {
# 		if ( is(ref, "MSImagingExperiment") || is(ref, "MassDataFrame") )
# 			ref <- mz(ref)
# 	}
# 	if ( missing(units) && !missing(tolerance) )
# 		units <- get_units_from_names(tolerance, units)
# 	spectraData <- spectraData(ans)
# 	featureData <- as(featureData(ans), "MassDataFrame")
# 	new("MSImagingExperiment", spectraData=spectraData,
# 		featureData=featureData, elementMetadata=pixelData(ans),
# 		experimentData=experimentData(object),
# 		centroided=TRUE, metadata=metadata(ans))
# })

# .peakAlign_MSImagingArrays <- function(object, ref,
# 	binfun, binratio, tolerance, units, f,
# 	verbose, BPPARAM, ...)
# {
# 	tol.ref <- switch(units, relative="x", absolute="abs")
# 	if ( is.null(domain) || is.na(tolerance) ) {
# 		width <- match.arg(binfun, c("median", "min", "max", "mean"))
# 		.Log("summarizing peak gaps for alignment",
# 			message=verbose)
# 		.Log("using bin function ", sQuote(width),
# 			" to summarize peak gaps across spectra",
# 			message=verbose)
# 		indexbins <- estimateDomain(index, width=width, units=units,
# 			verbose=verbose, chunkopts=chunkopts, BPPARAM=BPPARAM)
# 	}
# 	if ( is.na(tolerance) ) {
# 		# estimate tolerance as (binratio x min peak-to-peak gap)
# 		tol <- binratio * estres(indexbins, ref=tol.ref)
# 		tol <- switch(units,
# 			relative=round(2 * tol, digits=6L) * 0.5,
# 			absolute=round(tol, digits=4L))
# 		.Log("estimated ", units, " tolerance of ", tol,
# 			message=verbose)
# 	} else {
# 		# validate user-specified tolerance
# 		tol <- setNames(unname(tolerance), units)
# 	}
# 	if ( is.null(domain) ) {
# 		# set peak bins estimated from index
# 		res <- estres(indexbins, ref=tol.ref)
# 		res <- switch(units,
# 			relative=round(2 * res, digits=6L) * 0.5,
# 			absolute=round(res, digits=4L))
# 		domain <- indexbins
# 	} else {
# 		# set peak bins to (tolerance / binratio)
# 		res <- tol / binratio
# 		domain <- switch(units,
# 			relative=seq_rel(min(domain), max(domain), by=res),
# 			absolute=seq(min(domain), max(domain), by=res))
# 	}
# 	if ( missing(ref) || is.null(ref) ) {
# 		.Log("using bin ratio of ", binratio,
# 			" to create peak bins (per tolerance half-window)",
# 			message=verbose)
# 		.Log("using peak bins with ", units,
# 			" resolution of ", res,
# 			message=verbose)
# 		.Log("binning peaks to create shared reference",
# 			message=verbose)
# 		FUN <- isofun(function(x, domain, tol, tol.ref) {
# 			matter::binpeaks(x, domain=domain, tol=tol, tol.ref=tol.ref,
# 				merge=FALSE, na.drop=FALSE)
# 		}, CardinalEnv())
# 		peaks <- chunk_lapply(index, FUN,
# 			domain=domain, tol=tol, tol.ref=tol.ref,
# 			simplify=matter::stat_c,
# 			verbose=verbose, chunkopts=chunkopts,
# 			BPPARAM=BPPARAM)
# 		.Log("merging peak bins with ", units,
# 			" centroid differences <= ", tol,
# 			message=verbose)
# 		peaks <- peaks[!is.na(peaks)]
# 		peaks <- mergepeaks(peaks, tol=tol, tol.ref=tol.ref)
# 		n <- nobs(peaks)
# 		ref <- structure(as.vector(peaks), n=n)
# 	} else {
# 		n <- NULL
# 	}
# 	if ( verbose ) {
# 		ppm <- switch(units,
# 			relative=paste0("(", 1e6 * tol, " ppm)"),
# 			absolute="")
# 		.Log("aligned to ", length(ref),
# 			" reference peaks with ", units,
# 			" tolerance ", tol, " ", ppm,
# 			message=verbose)
# 	}
# 	spectra <- sparse_mat(index=index,
# 		data=spectra, domain=ref,
# 		nrow=length(ref), ncol=length(object),
# 		tolerance=tol, sampler="none")
# 	spectraData <- SpectraArrays(setNames(list(spectra), spectraname))
# 	featureData <- DataFrame(setNames(list(ref), indexname))
# 	if ( !is.null(n) ) {
# 		featureData[["count"]] <- n
# 		featureData[["freq"]] <- n / length(object)
# 	}
# 	label <- "peak alignment"
# 	metadata <- list(
# 		tolerance=unname(tol), units=units,
# 		binfun=binfun, binratio=binratio)
# 	metadata <- setNames(list(metadata), label)
# 	metadata <- setNames(list(metadata), .processing_id())
# 	metadata <- c(metadata(object), metadata)
# 	names(metadata) <- make.unique(names(metadata))
# 	new("SpectralImagingExperiment", spectraData=spectraData,
# 		featureData=featureData, elementMetadata=pixelData(object),
# 		metadata=metadata, processing=list())
# }
