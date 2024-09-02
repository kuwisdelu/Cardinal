
#### Row/column apply ####
## ------------------------

setMethod("spectrapply", "SpectralImagingExperiment",
	function(object, FUN, ...,
		spectra = "intensity", index = NULL,
		simplify = TRUE, outpath = NULL,
		verbose = getCardinalVerbose(), chunkopts = list(),
		BPPARAM = getCardinalBPPARAM())
	{
		if ( length(processingData(object)) > 0L )
			.Warn("processing steps will be ignored by spectrapply()")
		if ( length(index) > 1L )
			.Error("more than 1 'index' array not allowed ",
				"for class ", sQuote(class(object)[1L]))
		nindex <- max(0L, length(index))
		snm <- spectra
		inm <- index
		spectra <- spectra(object, snm)
		if ( !is.null(index) ) {
			index <- featureData(object)[[inm[1L]]]
			if ( is.null(index) )
				.Error("index ", sQuote(inm[1L]), " not found")
		}
		if ( ...length() > 0L && is.null(...names()) )
			.Error("spectrapply() arguments passed via ... must be named")
		if ( nindex == 0L ) {
			chunkApply(spectra, 2L, FUN, ...,
				simplify=simplify, outpath=outpath,
				verbose=verbose, chunkopts=chunkopts,
				BPPARAM=BPPARAM)
		} else {
			chunkApply(spectra, 2L, FUN, index, ...,
				simplify=simplify, outpath=outpath,
				verbose=verbose, chunkopts=chunkopts,
				BPPARAM=BPPARAM)
		}
	})

setMethod("spectrapply", "SpectralImagingArrays",
	function(object, FUN, ...,
		spectra = "intensity", index = NULL,
		simplify = TRUE, outpath = NULL,
		verbose = getCardinalVerbose(), chunkopts = list(),
		BPPARAM = getCardinalBPPARAM())
	{
		if ( length(processingData(object)) > 0L )
			.Warn("processing steps will be ignored by spectrapply()")
		if ( length(index) > 3L )
			.Error("more than 3 'index' arrays not allowed ",
				"for class ", sQuote(class(object)[1L]))
		nindex <- max(0L, length(index))
		snm <- spectra
		inm <- index
		spectra <- spectra(object, snm)
		if ( !is.null(index) ) {
			index <- spectra(object, inm[1L])
			if ( is.null(index) )
				.Error("index ", sQuote(inm[1L]), " not found")
			if ( nindex >= 2L ) {
				index2 <- spectra(object, inm[2L])
				if ( is.null(index2) )
					.Error("index ", sQuote(inm[2L]), " not found")
			}
			if ( nindex >= 3L ) {
				index3 <- spectra(object, inm[3L])
				if ( is.null(index2) )
					.Error("index ", sQuote(inm[3L]), " not found")
			}
		}
		if ( ...length() > 0L && is.null(...names()) )
			.Error("spectrapply() arguments passed via ... must be named")
		if ( nindex == 0L ) {
			chunkLapply(spectra, FUN, ...,
				simplify=simplify, outpath=outpath,
				verbose=verbose, chunkopts=chunkopts,
				BPPARAM=BPPARAM)
		} else {
			args <- list(...)
			if ( nindex == 1L ) {
				chunkMapply(FUN, spectra, index, MoreArgs=args,
					simplify=simplify, outpath=outpath,
					verbose=verbose, chunkopts=chunkopts,
					BPPARAM=BPPARAM)
			} else if ( nindex == 2L ) {
				chunkMapply(FUN, spectra, index, index2, MoreArgs=args,
					simplify=simplify, outpath=outpath,
					verbose=verbose, chunkopts=chunkopts,
					BPPARAM=BPPARAM)
			} else if ( nindex == 3L ) {
				chunkMapply(FUN, spectra, index, index2, index3, MoreArgs=args,
					simplify=simplify, outpath=outpath,
					verbose=verbose, chunkopts=chunkopts,
					BPPARAM=BPPARAM)
			} else {
				.Error("too many 'index' arrays")
			}
		}
	})

