
## Summarize

setMethod("rowStats", "SpectralImagingExperiment",
	function(x, stat, ..., BPPARAM = bpparam()) {
		rowStats(spectra(x), stat=stat,
			nchunks=getCardinalNChunks(),
			verbose=getCardinalVerbose(),
			BPPARAM=getCardinalBPPARAM(), ...)
	})

setMethod("colStats", "SpectralImagingExperiment",
	function(x, stat, ..., BPPARAM = bpparam()) {
		colStats(spectra(x), stat=stat,
			nchunks=getCardinalNChunks(),
			verbose=getCardinalVerbose(),
			BPPARAM=getCardinalBPPARAM(), ...)
	})

summarizeFeatures <- function(x, stat = "mean", ...) {
	if ( "FUN" %in% ...names() ) {
		.Deprecated(old="FUN", new="stat")
		stat <- list(...)$FUN
	}
	if ( length(stat) != 1L )
		stop("stat must be a single string")
	nm <- if (is.null(names(stat))) stat else names(stat)
	ans <- rowStats(x, stat=stat, ...)
	if ( is.array(ans) ) {
		featureData(x)[colnames(ans)] <- ans
	} else {
		featureData(x)[[nm]] <- ans
	}
	x
}

summarizePixels <- function(x, stat = c(tic="sum"), ...) {
	if ( "FUN" %in% ...names() ) {
		.Deprecated(old="FUN", new="stat")
		stat <- list(...)$FUN
	}
	if ( length(stat) != 1L )
		stop("stat must be a single string")
	nm <- if (is.null(names(stat))) stat else names(stat)
	ans <- colStats(x, stat=stat, ...)
	if ( is.array(ans) ) {
		pixelData(x)[colnames(ans)] <- ans
	} else {
		pixelData(x)[[nm]] <- ans
	}
	x
}
