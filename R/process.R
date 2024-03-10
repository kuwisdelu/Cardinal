
#### Spectral pre-processing ####
## ------------------------------

setMethod("addProcessing", "SpectralImagingData",
	function(object, FUN, label, ...)
{
	ps <- ProcessingStep(FUN, ARGS=list(...))
	ps <- setNames(list(ps), label)
	processingData(object) <- c(processingData(object), ps)
	if ( validObject(object) )
		object
})

## Process MSImagingExperiment spectra

setMethod("process", "MSImagingExperiment",
	function(object, spectra = "intensity", domain = "mz", ...)
{
	try({
		pnm <- names(processingData(object))
		experimentData(object)[["dataProcessing"]] <- pnm
	}, silent=TRUE)
	callNextMethod(object, spectra=spectra, domain=domain, ...)
})

setMethod("process", "SpectralImagingExperiment",
	function(object, spectra = "intensity", domain = NULL,
		outfile = NULL, BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( length(processingData(object)) == 0L )
		return(object)
	ps <- processingData(object)
	if ( getCardinalVerbose() )
		message("processing ", paste0(names(ps), collapse=", "))
	if ( !is.null(outfile) ) {
		outfile <- normalizePath(outfile, mustWork=FALSE)
		pid <- ipcid()
		put <- matter:::chunk_writer(pid, outfile)
		if ( getCardinalVerbose() )
			message("writing output to path = ", sQuote(outfile))
	}
	snm <- spectra
	dnm <- domain
	spectra <- spectra(object, snm)
	if ( is.null(dnm) ) {
		dnm <- "index"
		domain <- seq_len(nrow(object))
	} else {
		domain <- featureData(object)[[dnm]]
	}
	FUN <- function(X, T)
	{
		ans <- vector("list", ncol(X))
		for ( i in seq_len(ncol(X)) )
		{
			x <- X[,i]
			t <- T
			for ( j in seq_along(ps) ) {
				y <- executeProcessingStep(ps[[j]], x, t)
				if ( is.matrix(y) ) {
					t <- y[,1L]
					x <- y[,2L]
				} else {
					x <- y
				}
			}
			if ( is.matrix(y) || !is.null(outfile) ) {
				ans[[i]] <- list(t, x)
			} else {
				ans[[i]] <- x
			}
		}
		if ( is.list(ans[[1L]]) )
			ans <- do.call(c, ans)
		if ( !is.null(outfile) ) {
			put(ans)
		} else {
			ans
		}
	}
	ans <- chunk_colapply(spectra, FUN, T=domain,
		nchunks=getCardinalNChunks(),
		verbose=getCardinalVerbose(),
		BPPARAM=BPPARAM)
	if ( length(ans) == length(object) ) {
		if ( is.matter(ans) ) {
			spectra(object, snm) <- as(ans, "matter_mat")
		} else {
			spectra(object, snm) <- do.call(cbind, ans)
		}
	} else if ( length(ans) == 2 * length(object) ) {
		drop <- if (is.matter(ans)) NULL else FALSE
		index <- ans[seq(1L, length(ans) - 1L, by=2L),drop=drop]
		spectra <- ans[seq(2L, length(ans), by=2L),drop=drop]
		spectra <- matter::sparse_mat(index=index,
			data=spectra, domain=domain,
			nrow=nrow(object), ncol=ncol(object),
			tolerance=matter::estres(domain),
			sampler="none")
		spectra(object, snm) <- spectra
	} else {
		stop("processed length [", length(ans), "] ",
			"does not match length of object [", length(object), "]")
	}
	if ( getCardinalVerbose() )
		message("output spectra: ", snm)
	processingData(object) <- list()
	if ( validObject(object) )
		object
})


## Process MSImagingArrays spectra

setMethod("process", "MSImagingArrays",
	function(object, spectra = "intensity", domain = "mz", ...)
{
	try({
		pnm <- names(processingData(object))
		experimentData(object)[["dataProcessing"]] <- pnm
	}, silent=TRUE)
	callNextMethod(object, spectra=spectra, domain=domain, ...)
})

setMethod("process", "SpectralImagingArrays",
	function(object, spectra = "intensity", domain = NULL,
		outfile = NULL, BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( length(processingData(object)) == 0L )
		return(object)
	ps <- processingData(object)
	if ( getCardinalVerbose() )
		message("processing ", paste0(names(ps), collapse=", "))
	if ( !is.null(outfile) ) {
		outfile <- normalizePath(outfile, mustWork=FALSE)
		pid <- ipcid()
		put <- matter:::chunk_writer(pid, outfile)
		if ( getCardinalVerbose() )
			message("writing output to path = ", sQuote(outfile))
	}
	snm <- spectra
	dnm <- domain
	spectra <- spectra(object, snm)
	if ( is.null(dnm) ) {
		dnm <- "index"
		domain <- lapply(lengths(spectra), seq_len)
	} else {
		domain <- spectra(object, dnm)
	}
	FUN <- function(X, T, MoreArgs)
	{
		ans <- vector("list", length(X))
		for ( i in seq_along(X) )
		{
			x <- X[[i]]
			t <- T[[i]]
			for ( j in seq_along(ps) ) {
				y <- executeProcessingStep(ps[[j]], x, t)
				if ( is.matrix(y) ) {
					t <- y[,1L]
					x <- y[,2L]
				} else {
					x <- y
				}
			}
			if ( is.matrix(y) || !is.null(outfile) ) {
				ans[[i]] <- list(t, x)
			} else {
				ans[[i]] <- x
			}
		}
		if ( is.list(ans[[1L]]) )
			ans <- do.call(c, ans)
		if ( !is.null(outfile) ) {
			put(ans)
		} else {
			ans
		}
	}
	ans <- chunk_mapply(FUN, spectra, domain,
		nchunks=getCardinalNChunks(),
		verbose=getCardinalVerbose(),
		BPPARAM=BPPARAM)
	if ( length(ans) == length(object) ) {
		spectra(object, snm) <- ans
		if ( getCardinalVerbose() )
			message("output spectra: ", snm)
	} else if ( length(ans) == 2 * length(object) ) {
		drop <- if (is.matter(ans)) NULL else FALSE
		domain <- ans[seq(1L, length(ans) - 1L, by=2L),drop=drop]
		spectra <- ans[seq(2L, length(ans), by=2L),drop=drop]
		spectra(object, dnm) <- domain
		spectra(object, snm) <- spectra
		if ( getCardinalVerbose() )
			message("output spectra: ", paste0(c(dnm, snm), collapse=", "))
	} else {
		stop("processed length [", length(ans), "] ",
			"does not match length of object [", length(object), "]")
	}
	processingData(object) <- list()
	experimentData(object)[["dataProcessing"]] <- names(ps)
	if ( validObject(object) )
		object
})
