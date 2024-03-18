
#### Row/column apply ####
## ------------------------

setMethod("spectrapply", "MSImagingExperiment",
	function(object, FUN, ...,
		spectra = "intensity", index = "mz",
		BPPARAM = getCardinalBPPARAM())
	{
		callNextMethod(object, FUN=FUN, ...,
			spectra=spectra, index=index,
			BPPARAM=BPPARAM)
	})

setMethod("spectrapply", "MSImagingArrays",
	function(object, FUN, ...,
		spectra = "intensity", index = "mz",
		BPPARAM = getCardinalBPPARAM())
	{
		callNextMethod(object, FUN=FUN, ...,
			spectra=spectra, index=index,
			BPPARAM=BPPARAM)
	})

setMethod("spectrapply", "SpectralImagingExperiment",
	function(object, FUN, ...,
		spectra = "intensity", index = NULL,
		simplify = TRUE, outpath = NULL,
		nchunks = getCardinalNChunks(),
		verbose = getCardinalVerbose(),
		BPPARAM = getCardinalBPPARAM())
	{
		if ( length(processingData(object)) > 0L )
			warning("processing steps will be ignored by spectrapply()")
		xnm <- spectra
		tnm <- index
		spectra <- spectra(object, xnm)
		if ( is.null(tnm) ) {
			tnm <- "index"
			index <- seq_len(nrow(object))
		} else {
			index <- featureData(object)[[tnm]]
			if ( is.null(index) )
				stop("index ", sQuote(tnm), " not found")
		}
		if ( ...length() > 0L && is.null(...names()) )
			stop("spectrapply() arguments passed via ... must be named")
		FUN2 <- function(x, ...) FUN(x, index, ...)
		chunkApply(spectra, FUN=FUN2, MARGIN=2L, ...,
			simplify=simplify, outpath=outpath,
			nchunks=nchunks, verbose=verbose,
			BPPARAM=BPPARAM)
	})

setMethod("spectrapply", "SpectralImagingArrays",
	function(object, FUN, ...,
		spectra = "intensity", index = NULL,
		simplify = TRUE, outpath = NULL,
		nchunks = getCardinalNChunks(),
		verbose = getCardinalVerbose(),
		BPPARAM = getCardinalBPPARAM())
	{
		if ( length(processingData(object)) > 0L )
			warning("processing steps will be ignored by spectrapply()")
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
		if ( ...length() > 0L && is.null(...names()) )
			stop("spectrapply() arguments passed via ... must be named")
		args <- list(...)
		chunkMapply(FUN, spectra, index, MoreArgs=args,
			simplify=simplify, outpath=outpath,
			nchunks=nchunks, verbose=verbose,
			BPPARAM=BPPARAM)
	})

#### Row/column statistics ####
## ----------------------------

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

setMethod("rowStats", "SpectralImagingExperiment",
	function(x, stat, groups = NULL, ...,
		BPPARAM = getCardinalBPPARAM())
	{
		rowStats(spectra(x), stat=stat, groups=groups,
			nchunks=getCardinalNChunks(),
			verbose=getCardinalVerbose(),
			BPPARAM=BPPARAM, ...)
	})

setMethod("colStats", "SpectralImagingExperiment",
	function(x, stat, groups = NULL, ...,
		BPPARAM = getCardinalBPPARAM())
	{
		colStats(spectra(x), stat=stat, groups=groups,
			nchunks=getCardinalNChunks(),
			verbose=getCardinalVerbose(),
			BPPARAM=BPPARAM, ...)
	})


#### Summarization ####
## ---------------------

summarizeFeatures <- function(x, stat = "mean",
	groups = NULL, BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( "FUN" %in% ...names() ) {
		.Deprecated(old="FUN", new="stat")
		stat <- list(...)$FUN
	}
	if ( is(x, "MSImagingArrays") ) {
		x <- convertMSImagingArrays2Experiment(x, BPPARAM=BPPARAM)
	} else {
		x <- as(x, "SpectralImagingExperiment", strict=FALSE)
	}
	if ( is.null(names(stat)) ) {
		labels <- stat
	} else {
		labels <- ifelse(nchar(names(stat)), names(stat), stat)
	}
	ans <- rowStats(x, stat=stat, group=groups,
		simplify=FALSE, BPPARAM=BPPARAM, ...)
	for ( i in seq_along(ans) ) {
		y <- matter:::drop_attr(ans[[i]])
		if ( is.array(y) ) {
			nm <- paste0(colnames(y), ".", labels[i])
			featureData(x)[nm] <- y
		} else {
			featureData(x)[[labels[i]]] <- y
		}
	}
	x
}

summarizePixels <- function(x, stat = c(tic="sum"),
	groups = NULL, BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( "FUN" %in% ...names() ) {
		.Deprecated(old="FUN", new="stat")
		stat <- list(...)$FUN
	}
	if ( is(x, "MSImagingArrays") ) {
		x <- convertMSImagingArrays2Experiment(x, BPPARAM=BPPARAM)
	} else {
		x <- as(x, "SpectralImagingExperiment", strict=FALSE)
	}
	if ( is.null(names(stat)) ) {
		labels <- stat
	} else {
		labels <- ifelse(nchar(names(stat)), names(stat), stat)
	}
	ans <- colStats(x, stat=stat, group=groups,
		simplify=FALSE, BPPARAM=BPPARAM, ...)
	for ( i in seq_along(ans) ) {
		y <- matter:::drop_attr(ans[[i]])
		if ( is.array(y) ) {
			nm <- paste0(colnames(y), ".", labels[i])
			pixelData(x)[nm] <- y
		} else {
			pixelData(x)[[labels[i]]] <- y
		}
	}
	x
}
