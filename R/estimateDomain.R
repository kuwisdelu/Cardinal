
#### Estimate shared domain ####
## ------------------------------

estimateDomain <- function(xlist,
	width = c("median", "min", "max", "mean"),
	units = c("relative", "absolute"))
{
	width <- match.arg(width)
	units <- match.arg(units)
	ref <- switch(units, relative="x", absolute="abs")
	summaries <- lapply(xlist, .computeDomainSummary, ref=ref)
	.aggregateDomainSummaries(summaries, width, units)
}

.computeDomainSummary <- function(x, ref)
{
	x <- x[!is.na(x)]
	res <- unname(estres(x, ref=ref))
	if ( length(x) > 0 && is.finite(res) ) {
		c(min=min(x), max=max(x), res=res)
	} else {
		c(min=NA_real_, max=NA_real_, res=res)
	}
}

.aggregateDomainSummaries <- function(xlist, width, units)
{
	xlist <- do.call(rbind, xlist)
	colnames(xlist) <- c("min", "max", "res")
	from <- floor(min(xlist[,"min"], na.rm=TRUE))
	to <- ceiling(max(xlist[,"max"], na.rm=TRUE))
	by <- match.fun(width)(xlist[,"res"], na.rm=TRUE)
	minRelativeRes <- 5e-7 # == 0.5 ppm
	minAbsoluteRes <- 1e-4 # == 0.0001
	by <- switch(units,
		relative=max(minRelativeRes, round(2 * by, digits=6L) * 0.5),
		absolute=max(minAbsoluteRes, round(by, digits=4L)))
	domain <- switch(units,
		relative=seq_rel(from, to, by=by),
		absolute=seq.default(from, to, by=by))
	structure(as.vector(domain),
		resolution = setNames(by, units))
}

estimateReferenceMz <- function(object,
	width = c("median", "min", "max", "mean"),
	units = c("ppm", "mz"),
	f = processingChunkFactor(object),
	verbose = getCardinalVerbose(),
	BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( is(object, "MSImagingExperiment") || is(object, "MassDataFrame") ) {
		mz(object)
	} else if ( is(object, "MSImagingArrays") ) {
		width <- match.arg(width)
		units <- match.arg(units)
		units <- switch(units, ppm="relative", mz="absolute")
		ref <- switch(units, relative="x", absolute="abs")
		summaries <- spectrapply(object,
			FUN=.computeMzSummary, ref=ref,
			f=f, verbose=verbose, BPPARAM=BPPARAM, ...)
		.aggregateDomainSummaries(summaries, width, units)
	} else {
		.Error("can't estimate m/z values for class ", sQuote(class(object)))
	}
}

.computeMzSummary <- function(x, ref)
{
	.computeDomainSummary(x$mz, ref)
}

estimateReferencePeaks <- function(object, SNR = 2,
	method = c("diff", "sd", "mad", "quantile", "filter", "cwt"),
	verbose = getCardinalVerbose(), chunkopts = list(),
	BPPARAM = getCardinalBPPARAM(), ...)
{
	method <- match.arg(method)
	if ( is(object, "MSImagingArrays") ) {
		if ( length(processingData(object)) > 0L )
			.Warn("queued processing steps will be ignored")
		object <- convertMSImagingArrays2Experiment(object,
			verbose=verbose, chunkopts=chunkopts,
			BPPARAM=BPPARAM, ...)
	}
	object <- summarizeFeatures(object, stat="mean",
		verbose=verbose, chunkopts=chunkopts,
		BPPARAM=BPPARAM)
	featureData <- featureData(object)
	peaks <- findpeaks(featureData[["mean"]], noise=method, snr=SNR, ...)
	featureData[peaks,,drop=FALSE]
}

