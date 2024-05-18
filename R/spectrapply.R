
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
		nindex <- max(1L, length(index))
		snm <- spectra
		inm <- index
		spectra <- spectra(object, snm)
		if ( is.null(inm) ) {
			inm <- "index"
			index <- seq_len(nrow(object))
		} else {
			index <- featureData(object)[[inm[1L]]]
			if ( is.null(index) )
				stop("index ", sQuote(inm[1L]), " not found")
			if ( nindex > 1L ) {
				index2 <- featureData(object)[[inm[2L]]]
				if ( is.null(index2) )
					stop("index ", sQuote(inm[2L]), " not found")
			}
		}
		if ( ...length() > 0L && is.null(...names()) )
			stop("spectrapply() arguments passed via ... must be named")
		if ( nindex == 1L ) {
			chunkApply(spectra, 2L, FUN, index, ...,
				simplify=simplify, outpath=outpath,
				nchunks=nchunks, verbose=verbose,
				BPPARAM=BPPARAM)
		} else if ( nindex == 2L ) {
			chunkApply(spectra, 2L, FUN, index, index2, ...,
				simplify=simplify, outpath=outpath,
				nchunks=nchunks, verbose=verbose,
				BPPARAM=BPPARAM)
		} else {
			stop("more than 2 'index' arrays not allowed")
		}
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
		nindex <- max(1L, length(index))
		snm <- spectra
		inm <- index
		spectra <- spectra(object, snm)
		if ( is.null(inm) ) {
			inm <- "index"
			index <- lapply(lengths(spectra), seq_len)
		} else {
			index <- spectra(object, inm[1L])
			if ( is.null(index) )
				stop("index ", sQuote(inm[1L]), " not found")
			if ( nindex > 1L ) {
				index2 <- spectra(object, inm[2L])
				if ( is.null(index2) )
					stop("index ", sQuote(inm[2L]), " not found")
			}
		}
		if ( ...length() > 0L && is.null(...names()) )
			stop("spectrapply() arguments passed via ... must be named")
		args <- list(...)
		if ( nindex == 1L ) {
			chunkMapply(FUN, spectra, index, MoreArgs=args,
				simplify=simplify, outpath=outpath,
				nchunks=nchunks, verbose=verbose,
				BPPARAM=BPPARAM)
		} else if ( nindex == 2L ) {
			chunkMapply(FUN, spectra, index, index2, MoreArgs=args,
				simplify=simplify, outpath=outpath,
				nchunks=nchunks, verbose=verbose,
				BPPARAM=BPPARAM)
		} else {
			stop("more than 2 'index' arrays not allowed")
		}
	})

