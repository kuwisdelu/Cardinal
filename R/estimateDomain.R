
#### Estimate shared domain ####
## ------------------------------

estimateDomain <- function(xlist,
	units = c("relative", "absolute"),
	nchunks = getCardinalNChunks(),
	verbose = getCardinalVerbose(),
	BPPARAM = getCardinalBPPARAM(), ...)
{
	units <- match.arg(units)
	ref <- switch(units, relative="x", absolute="abs")
	FUN <- function(x) {
		x <- x[!is.na(x)]
		res <- unname(estres(x, ref=ref))
		c(min=min(x), max=max(x), res=res)
	}
	ans <- chunkLapply(xlist, FUN,
		nchunks=nchunks, verbose=verbose,
		BPPARAM=BPPARAM, ...)
	ans <- do.call(rbind, ans)
	from <- floor(min(ans[,1L], na.rm=TRUE))
	to <- ceiling(max(ans[,2L], na.rm=TRUE))
	by <- median(ans[,3L], na.rm=TRUE)
	by <- switch(units,
		relative=round(2 * by, digits=6L) * 0.5,
		absolute=round(by, digits=4L))
	ans <- switch(units,
		relative=seq_rel(from, to, by=by),
		absolute=seq.default(from, to, by=by))
	structure(as.vector(ans),
		resolution = setNames(by, units))
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
	if ( !"mean" %in% names(featureData(object)) )
	{
		object <- summarizeFeatures(object, stat="mean",
			nchunks=nchunks, verbose=verbose,
			BPPARAM=BPPARAM)
	}
	featureData <- featureData(object)
	peaks <- findpeaks(featureData[["mean"]], noise=method, snr=SNR, ...)
	featureData[peaks,,drop=FALSE]
}

