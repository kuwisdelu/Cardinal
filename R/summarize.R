
#### Summarize features (masses) ####
## ---------------------------------

summarizeFeatures <- function(x, stat = "mean", groups = NULL,
	verbose = getCardinalVerbose(), chunkopts = list(),
	BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( "FUN" %in% ...names() ) {
		.Deprecated(old="FUN", new="stat")
		stat <- list(...)$FUN
	}
	if ( is(x, "MSImagingArrays") ) {
		x <- convertMSImagingArrays2Experiment(x,
			verbose=verbose, chunkopts=chunkopts, BPPARAM=BPPARAM)
	} else {
		x <- as(x, "SpectralImagingExperiment", strict=FALSE)
	}
	if ( is.null(names(stat)) ) {
		labels <- stat
	} else {
		labels <- ifelse(nchar(names(stat)), names(stat), stat)
	}
	ans <- rowStats(x, stat=stat, group=groups, simplify=FALSE,
		verbose=verbose, chunkopts=chunkopts,
		BPPARAM=BPPARAM, ...)
	for ( i in seq_along(ans) ) {
		y <- as.vector(ans[[i]])
		if ( is.null(dim(ans[[i]])) ) {
			names(y) <- names(ans[[i]])
		} else {
			dim(y) <- dim(ans[[i]])
			dimnames(y) <- dimnames(ans[[i]])
		}
		if ( is.array(y) ) {
			nm <- paste0(colnames(y), ".", labels[i])
			featureData(x)[nm] <- y
		} else {
			featureData(x)[[labels[i]]] <- y
		}
	}
	x
}


#### Summarize pixels (spectra) ####
## --------------------------------

summarizePixels <- function(x, stat = c(tic="sum"), groups = NULL,
	verbose = getCardinalVerbose(), chunkopts = list(),
	BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( "FUN" %in% ...names() ) {
		.Deprecated(old="FUN", new="stat")
		stat <- list(...)$FUN
	}
	if ( is(x, "MSImagingArrays") ) {
		x <- convertMSImagingArrays2Experiment(x,
			verbose=verbose, chunkopts=chunkopts, BPPARAM=BPPARAM)
	} else {
		x <- as(x, "SpectralImagingExperiment", strict=FALSE)
	}
	if ( is.null(names(stat)) ) {
		labels <- stat
	} else {
		labels <- ifelse(nchar(names(stat)), names(stat), stat)
	}
	ans <- colStats(x, stat=stat, group=groups, simplify=FALSE,
		verbose=verbose, chunkopts=chunkopts,
		BPPARAM=BPPARAM, ...)
	for ( i in seq_along(ans) ) {
		y <- as.vector(ans[[i]])
		if ( is.null(dim(ans[[i]])) ) {
			names(y) <- names(ans[[i]])
		} else {
			dim(y) <- dim(ans[[i]])
			dimnames(y) <- dimnames(ans[[i]])
		}
		if ( is.array(y) ) {
			nm <- paste0(colnames(y), ".", labels[i])
			pixelData(x)[nm] <- y
		} else {
			pixelData(x)[[labels[i]]] <- y
		}
	}
	x
}


#### Row/column statistics ####
## ----------------------------

setMethod("rowStats", "SpectralImagingExperiment",
	function(x, stat, ...,
		verbose = getCardinalVerbose(), chunkopts = list(),
		BPPARAM = getCardinalBPPARAM())
	{
		rowStats(spectra(x), stat=stat,
			verbose=verbose, chunkopts=chunkopts,
			BPPARAM=BPPARAM, ...)
	})

setMethod("colStats", "SpectralImagingExperiment",
	function(x, stat, ...,
		verbose = getCardinalVerbose(), chunkopts = list(),
		BPPARAM = getCardinalBPPARAM())
	{
		colStats(spectra(x), stat=stat,
			verbose=verbose, chunkopts=chunkopts,
			BPPARAM=BPPARAM, ...)
	})

setMethod("rowSums", "SpectralImagingExperiment",
	function(x, na.rm = FALSE, dims = 1, ...)
	{
		rowStats(x, stat="sum", ..., na.rm=na.rm)
	})

setMethod("colSums", "SpectralImagingExperiment",
	function(x, na.rm = FALSE, dims = 1, ...)
	{
		colStats(x, stat="sum", ..., na.rm=na.rm)
	})

setMethod("rowMeans", "SpectralImagingExperiment",
	function(x, na.rm = FALSE, dims = 1, ...)
	{
		rowStats(x, stat="mean", ..., na.rm=na.rm)
	})

setMethod("colMeans", "SpectralImagingExperiment",
	function(x, na.rm = FALSE, dims = 1, ...)
	{
		colStats(x, stat="mean", ..., na.rm=na.rm)
	})


