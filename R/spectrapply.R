
#### Row/column apply ####
## ------------------------

setMethod("spectrapply", "MSImagingExperiment",
	function(object, FUN, ...,
		spectra = "intensity", index = "mz")
	{
		callNextMethod(object, FUN=FUN, ...,
			spectra=spectra, index=index)
	})

setMethod("spectrapply", "MSImagingArrays",
	function(object, FUN, ...,
		spectra = "intensity", index = "mz")
	{
		callNextMethod(object, FUN=FUN, ...,
			spectra=spectra, index=index)
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

