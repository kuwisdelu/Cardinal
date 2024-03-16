
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


# MSImagingExperiment

setMethod("process", "MSImagingExperiment",
	function(object, spectra = "intensity", index = "mz", domain = NULL,
		outfile = NULL, BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( !is.null(outfile) ) {
		outfile <- normalizePath(outfile, mustWork=FALSE)
		outfile <- paste0(tools::file_path_sans_ext(outfile), ".ibd")
		uuid <- matter_vec(uuid(uppercase=FALSE)$bytes,
			path=outfile, type="raw", readonly=FALSE)
	}
	for ( label in names(processingData(object)) ) {
		dp <- experimentData(object)[["dataProcessing"]]
		dp <- unname(c(dp, label))
		try({
			experimentData(object)[["dataProcessing"]] <- dp
		}, silent=TRUE)
	}
	ans <- callNextMethod(object, spectra=spectra, index=index, domain=domain,
		outfile=outfile, BPPARAM=BPPARAM, ...)
	.postprocess_MSIMagingExperiment(ans, outfile, spectra)
})

.postprocess_MSIMagingExperiment <- function(object, outfile, xname)
{
	if ( !is.null(outfile) )
	{
		if ( is.sparse(spectra(object, xname)) ) {
			mz <- atomindex(spectra(object, xname))
			intensity <- atomdata(spectra(object, xname))
		} else {
			mz <- matter_vec(path=outfile, type="double",
				offset=16L, length=nrow(object))
			intensity <- spectra(object, xname)
		}
		outfile <- paste0(tools::file_path_sans_ext(outfile), ".imzML")
		ok <- writeImzML(object, file=outfile, mz=mz, intensity=intensity, asis=TRUE)
		newpath <- attr(ok, "outpath")[2L]
		if ( is.sparse(spectra(object, xname)) ) {
			path(atomindex(spectra(object, xname))) <- newpath
			path(atomdata(spectra(object, xname))) <- newpath
		} else {
			path(spectra(object, xname)) <- newpath
		}
	}
	object
}


# MSImagingArrays

setMethod("process", "MSImagingArrays",
	function(object, spectra = "intensity", index = "mz", domain = NULL,
		outfile = NULL, BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( !is.null(outfile) ) {
		outfile <- normalizePath(outfile, mustWork=FALSE)
		outfile <- paste0(tools::file_path_sans_ext(outfile), ".ibd")
		uuid <- matter_vec(uuid(uppercase=FALSE)$bytes,
			path=outfile, type="raw", readonly=FALSE)
	}
	for ( label in names(processingData(object)) ) {
		dp <- experimentData(object)[["dataProcessing"]]
		dp <- unname(c(dp, label))
		try({
			experimentData(object)[["dataProcessing"]] <- dp
		}, silent=TRUE)
	}
	ans <- callNextMethod(object, spectra=spectra, index=index, domain=domain,
		outfile=outfile, BPPARAM=BPPARAM, ...)
	if ( !is.null(domain) ) {
		ans <- convertMSImagingArrays2Experiment(ans, mz=domain)
		.postprocess_MSIMagingExperiment(ans, outfile, spectra)
	} else {
		.postprocess_MSIMagingArrays(ans, outfile, spectra, index)
	}
})

.postprocess_MSIMagingArrays <- function(object, outfile, xname, tname)
{
	if ( !is.null(outfile) )
	{
		mz <- spectra(object, tname)
		intensity <- spectra(object, xname)
		outfile <- paste0(tools::file_path_sans_ext(outfile), ".imzML")
		ok <- writeImzML(object, file=outfile, mz=mz, intensity=intensity, asis=TRUE)
		newpath <- attr(ok, "outpath")[2L]
		path(spectra(object, tname)) <- newpath
		path(spectra(object, xname)) <- newpath
	}
	object
}


# SpectralImagingExperiment

setMethod("process", "SpectralImagingExperiment",
	function(object, spectra = "intensity", index = NULL, domain = NULL,
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
	xnm <- spectra
	tnm <- index
	spectra <- spectra(object, xnm)
	if ( is.null(tnm) ) {
		tnm <- "index"
		index <- seq_len(nrow(object))
	} else {
		index <- featureData(object)[[tnm]]
	}
	FUN <- function(X, T)
	{
		cid <- attr(X, "chunkid")
		ans <- vector("list", ncol(X))
		for ( i in seq_len(ncol(X)) )
		{
			xi <- X[,i]
			ti <- T
			for ( j in seq_along(ps) ) {
				xj <- executeProcessingStep(ps[[j]], xi, ti)
				if ( is.matrix(xj) ) {
					ti <- xj[,1L]
					xi <- xj[,2L]
				} else {
					xi <- xj
				}
			}
			if ( isTRUE(all.equal(ti, domain, check.attributes=FALSE)) ) {
				ans[[i]] <- xi
			} else if ( is.matrix(xj) ) {
				ans[[i]] <- list(ti, xi)
			} else if ( length(xi) == nrow(X) ) {
				ans[[i]] <- xi
			} else {
				stop("length of output [", length(xi), "] ",
					"does not match input [", nrow(X), "]")
			}
		}
		if ( cid == 1L && !is.null(outfile) && !is.list(ans[[1L]]) )
		{
			if ( is.null(domain) ) {
				put(as.double(T))
			} else {
				put(as.double(domain))
			}
		}
		if ( is.list(ans[[1L]]) )
			ans <- do.call(c, ans)
		if ( !is.null(outfile) ) {
			put(ans, cid)
		} else {
			ans
		}
	}
	ans <- chunk_colapply(spectra, FUN, T=index,
		nchunks=getCardinalNChunks(),
		verbose=getCardinalVerbose(),
		BPPARAM=BPPARAM)
	object <- .postprocess_SpectralImagingExperiment(ans,
		object=object, domain=domain, xname=xnm, tname=tnm)
	processingData(object) <- list()
	if ( !is.null(outfile) )
		ipcremove(pid)
	object
})

.postprocess_SpectralImagingExperiment <- function(ans,
	object, domain, xname, tname)
{
	if ( length(ans) == length(object) ) {
		index <- featureData(object)[[tname]]
		if ( is.matter(ans) ) {
			spectra <- as(ans, "matter_mat")
		} else {
			spectra <- do.call(cbind, ans)
		}
	} else if ( length(ans) == 2L * length(object) ) {
		if ( is.null(domain) )
			domain <- featureData(object)[[tname]]
		drop <- if (is.matter(ans)) NULL else FALSE
		index <- ans[seq(1L, length(ans) - 1L, by=2L),drop=drop]
		spectra <- ans[seq(2L, length(ans), by=2L),drop=drop]
		spectra <- sparse_mat(index=index,
			data=spectra, domain=domain,
			nrow=nrow(object), ncol=ncol(object),
			tolerance=estres(domain),
			sampler="none")
	} else {
		stop("processed length [", length(ans), "] ",
			"does not match length of object [", length(object), "]")
	}
	if ( getCardinalVerbose() )
		message("output spectra: ", xname)
	if ( is.null(domain) || identical(domain, index) ) {
		spectra(object, xname) <- spectra
	} else {
		spectraData <- SpectraArrays(setNames(list(spectra), xname))
		featureData <- DataFrame(setNames(list(domain), tname))
		featureData <- as(featureData, class(featureData(object)))
		object <- new(class(object), object,
			spectraData=spectraData, featureData=featureData)
	}
	if ( validObject(object) )
		object
}


# SpectralImagingArrays

setMethod("process", "SpectralImagingArrays",
	function(object, spectra = "intensity", index = NULL, domain = NULL,
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
	xnm <- spectra
	tnm <- index
	spectra <- spectra(object, xnm)
	if ( is.null(tnm) ) {
		tnm <- "index"
		index <- lapply(lengths(spectra), seq_len)
	} else {
		index <- spectra(object, tnm)
	}
	FUN <- function(X, T, MoreArgs)
	{
		cid <- attr(X, "chunkid")
		ans <- vector("list", length(X))
		for ( i in seq_along(X) )
		{
			xi <- X[[i]]
			ti <- T[[i]]
			for ( j in seq_along(ps) ) {
				xj <- executeProcessingStep(ps[[j]], xi, ti)
				if ( is.matrix(xj) ) {
					ti <- xj[,1L]
					xi <- xj[,2L]
				} else {
					xi <- xj
				}
			}
			if ( isTRUE(all.equal(ti, domain, check.attributes=FALSE)) ) {
				ans[[i]] <- xi
			} else if ( is.matrix(xj) || !is.null(outfile) ) {
				ans[[i]] <- list(ti, xi)
			} else if ( length(xi) == length(X[[i]]) ) {
				ans[[i]] <- xi
			} else {
				stop("length of output [", length(xi), "] ",
					"does not match input [", length(X[[i]]), "]")
			}
		}
		if ( cid == 1L && !is.null(outfile) && !is.list(ans[[1L]]) )
		{
			if ( !is.null(domain) )
				put(as.double(domain))
		}
		if ( is.list(ans[[1L]]) )
			ans <- do.call(c, ans)
		if ( !is.null(outfile) ) {
			put(ans, cid)
		} else {
			ans
		}
	}
	ans <- chunk_mapply(FUN, spectra, index,
		nchunks=getCardinalNChunks(),
		verbose=getCardinalVerbose(),
		BPPARAM=BPPARAM)
	object <- .postprocess_SpectralImagingArrays(ans,
		object=object, domain=domain, xname=xnm, tname=tnm)
	processingData(object) <- list()
	if ( !is.null(outfile) )
		ipcremove(pid)
	object
})

.postprocess_SpectralImagingArrays <- function(ans,
	object, domain, xname, tname)
{
	if ( length(ans) == length(object) ) {
		if ( is.null(domain) ) {
			index <- NULL
		} else {
			index <- rep.int(list(domain), length(object))
			if ( is(object, "MSImagingExperiment") )
				object@continuous <- TRUE
		}
		spectra <- ans
	} else if ( length(ans) == 2L * length(object) ) {
		drop <- if (is.matter(ans)) NULL else FALSE
		index <- ans[seq(1L, length(ans) - 1L, by=2L),drop=drop]
		spectra <- ans[seq(2L, length(ans), by=2L),drop=drop]
	} else {
		stop("processed length [", length(ans), "] ",
			"does not match length of object [", length(object), "]")
	}
	if ( !is.null(index) ) {
		if ( getCardinalVerbose() )
			message("output index: ", tname)
		spectra(object, tname) <- index
	}
	if ( getCardinalVerbose() )
		message("output spectra: ", xname)
	spectra(object, xname) <- spectra
	if ( validObject(object) )
		object
}

