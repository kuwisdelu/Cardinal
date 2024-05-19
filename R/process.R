
#### Spectral pre-processing ####
## ------------------------------

setMethod("addProcessing", "SpectralImagingData",
	function(object, FUN, label, verbose = getCardinalVerbose(), ...)
{
	ps <- ProcessingStep(FUN, ARGS=list(...))
	ps <- setNames(list(ps), label)
	processingData(object) <- c(processingData(object), ps)
	if ( verbose ) {
		labels <- names(processingData(object))
		message("queued: ", paste0(labels, collapse=", "))
	}
	if ( validObject(object) )
		object
})

reset <- function(object, ...)
{
	processingData(object) <- list()
	object
}


# MSImagingExperiment

setMethod("process", "MSImagingExperiment",
	function(object, spectra = "intensity", index = "mz",
		domain = NULL, outfile = NULL, ...)
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
	ans <- callNextMethod(object, spectra=spectra, index=index,
		domain=domain, outfile=outfile, ...)
	.postprocess_MSIMagingExperiment(ans, outfile, spectra)
})

.postprocess_MSIMagingExperiment <- function(object, outfile, spectraname)
{
	if ( !is.null(outfile) )
	{
		if ( is.sparse(spectra(object, spectraname)) ) {
			mz <- atomindex(spectra(object, spectraname))
			intensity <- atomdata(spectra(object, spectraname))
		} else {
			mz <- matter_vec(path=outfile, type="double",
				offset=16L, length=nrow(object))
			intensity <- spectra(object, spectraname)
		}
		outfile <- paste0(tools::file_path_sans_ext(outfile), ".imzML")
		ok <- writeImzML(object, file=outfile, mz=mz, intensity=intensity, asis=TRUE)
		newpath <- attr(ok, "outpath")[2L]
		if ( is.sparse(spectra(object, spectraname)) ) {
			path(atomindex(spectra(object, spectraname))) <- newpath
			path(atomdata(spectra(object, spectraname))) <- newpath
		} else {
			path(spectra(object, spectraname)) <- newpath
		}
	}
	object
}


# MSImagingArrays

setMethod("process", "MSImagingArrays",
	function(object, spectra = "intensity", index = "mz",
		domain = NULL, outfile = NULL, ...)
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
	ans <- callNextMethod(object, spectra=spectra, index=index,
		domain=domain, outfile=outfile, ...)
	if ( !is.null(domain) ) {
		ans <- convertMSImagingArrays2Experiment(ans, mz=domain)
		.postprocess_MSIMagingExperiment(ans, outfile, spectra)
	} else {
		.postprocess_MSIMagingArrays(ans, outfile, spectra, index)
	}
})

.postprocess_MSIMagingArrays <- function(object, outfile, spectraname, indexname)
{
	if ( !is.null(outfile) )
	{
		mz <- spectra(object, indexname[1L])
		intensity <- spectra(object, spectraname)
		outfile <- paste0(tools::file_path_sans_ext(outfile), ".imzML")
		ok <- writeImzML(object, file=outfile, mz=mz, intensity=intensity, asis=TRUE)
		newpath <- attr(ok, "outpath")[2L]
		path(spectra(object, indexname[1L])) <- newpath
		path(spectra(object, spectraname)) <- newpath
	}
	object
}


# SpectralImagingExperiment

setMethod("process", "SpectralImagingExperiment",
	function(object, spectra = "intensity", index = NULL,
		domain = NULL, outfile = NULL,
		nchunks = getCardinalNChunks(),
		verbose = getCardinalVerbose(),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( length(processingData(object)) == 0L )
		return(object)
	ps <- processingData(object)
	if ( verbose )
		message("processing: ", paste0(names(ps), collapse=", "))
	if ( !is.null(outfile) ) {
		outfile <- normalizePath(outfile, mustWork=FALSE)
		pid <- ipcid()
		put <- matter::chunk_writer(pid, outfile)
		if ( verbose )
			message("writing output to path = ", sQuote(outfile))
	} else {
		put <- NULL
	}
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
	FUN <- .process_fun(ps, domain=domain, put=put)
	if ( nindex == 1L ) {
		ans <- chunk_colapply(spectra, FUN, index,
			nchunks=nchunks, verbose=verbose,
			BPPARAM=BPPARAM)
	} else if ( nindex == 2L ) {
		ans <- chunk_colapply(spectra, FUN, index, index2,
			nchunks=nchunks, verbose=verbose,
			BPPARAM=BPPARAM)
	} else {
		stop("more than 2 'index' arrays not allowed")
	}
	object <- .postprocess_SpectralImagingExperiment(ans,
		object=object, domain=domain, spectraname=snm, indexname=inm)
	metadata(object)[["processing"]] <- c(metadata(object)[["processing"]], ps)
	processingData(object) <- list()
	if ( !is.null(outfile) )
		ipcremove(pid)
	object
})

.postprocess_SpectralImagingExperiment <- function(ans,
	object, domain, spectraname, indexname)
{
	drop <- if (is.matter(ans)) NULL else FALSE
	if ( length(ans) == length(object) ) {
		if ( is.matter(ans) ) {
			spectra <- as(ans, "matter_mat")
		} else {
			spectra <- do.call(cbind, ans)
		}
		if ( is.null(domain) ) {
			index <- NULL
		} else {
			index <- featureData(object)[[indexname[1L]]]
		}
	} else {
		if ( is.null(domain) )
			domain <- featureData(object)[[indexname[1L]]]
		if ( length(ans) == 2L * length(object) ) {
			spectra <- ans[seq(2L, length(ans), by=2L),drop=drop]
			index <- ans[seq(1L, length(ans) - 1L, by=2L),drop=drop]
		} else {
			stop("processed length [", length(ans), "] ",
				"does not match length of object [", length(object), "]")
		}
		spectra <- sparse_mat(index=index,
			data=spectra, domain=domain,
			nrow=nrow(object), ncol=ncol(object),
			tolerance=estres(domain),
			sampler="none")
	}
	if ( getCardinalVerbose() )
		message("output spectra: ", spectraname)
	if ( is.null(domain) || identical(domain, index) ) {
		spectra(object, spectraname) <- spectra
	} else {
		spectraData <- SpectraArrays(setNames(list(spectra), spectraname))
		featureData <- DataFrame(setNames(list(domain), indexname[1L]))
		featureData <- as(featureData, class(featureData(object)))
		object <- new(class(object), object,
			spectraData=spectraData, featureData=featureData)
	}
	if ( validObject(object) )
		object
}


# SpectralImagingArrays

setMethod("process", "SpectralImagingArrays",
	function(object, spectra = "intensity", index = NULL,
		domain = NULL, outfile = NULL,
		nchunks = getCardinalNChunks(),
		verbose = getCardinalVerbose(),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( length(processingData(object)) == 0L )
		return(object)
	ps <- processingData(object)
	if ( verbose )
		message("processing: ", paste0(names(ps), collapse=", "))
	if ( !is.null(outfile) ) {
		outfile <- normalizePath(outfile, mustWork=FALSE)
		pid <- ipcid()
		put <- matter::chunk_writer(pid, outfile)
		if ( verbose )
			message("writing output to path = ", sQuote(outfile))
	} else {
		put <- NULL
	}
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
	FUN <- .process_fun(ps, domain=domain, put=put)
	if ( nindex == 1L ) {
		ans <- chunk_mapply(FUN, spectra, index,
			nchunks=nchunks, verbose=verbose,
			BPPARAM=BPPARAM)
	} else if ( nindex == 2L ) {
		ans <- chunk_mapply(FUN, spectra, index, index2,
			nchunks=nchunks, verbose=verbose,
			BPPARAM=BPPARAM)
	} else {
		stop("more than 2 'index' arrays not allowed")
	}
	object <- .postprocess_SpectralImagingArrays(ans,
		object=object, domain=domain, spectraname=snm, indexname=inm)
	metadata(object)[["processing"]] <- c(metadata(object)[["processing"]], ps)
	processingData(object) <- list()
	if ( !is.null(outfile) )
		ipcremove(pid)
	object
})

.postprocess_SpectralImagingArrays <- function(ans,
	object, domain, spectraname, indexname)
{
	drop <- if (is.matter(ans)) NULL else FALSE
	if ( length(ans) == length(object) ) {
		spectra <- ans
		if ( is.null(domain) ) {
			index <- NULL
		} else {
			index <- rep.int(list(domain), length(object))
			if ( .hasSlot(object, "continuous") )
				object@continuous <- TRUE
		}
		index2 <- NULL
	} else if ( length(ans) == 2L * length(object) ) {
		spectra <- ans[seq(2L, length(ans), by=2L),drop=drop]
		index <- ans[seq(1L, length(ans) - 1L, by=2L),drop=drop]
		index2 <- NULL
	} else if ( length(ans) == 3L * length(object) ) {
		spectra <- ans[seq(3L, length(ans), by=3L),drop=drop]
		index <- ans[seq(1L, length(ans) - 2L, by=3L),drop=drop]
		index2 <- ans[seq(2L, length(ans) - 1L, by=3L),drop=drop]
	} else {
		stop("processed length [", length(ans), "] ",
			"does not match length of object [", length(object), "]")
	}
	if ( !is.null(index) ) {
		if ( getCardinalVerbose() )
			message("output index: ", indexname[1L])
		spectra(object, indexname[1L]) <- index
	}
	if ( !is.null(index2) ) {
		if ( getCardinalVerbose() )
			message("output index: ", indexname[2L])
		spectra(object, indexname[2L]) <- index2
	}
	if ( getCardinalVerbose() )
		message("output spectra: ", spectraname)
	spectra(object, spectraname) <- spectra
	if ( validObject(object) )
		object
}

# process worker function

.process_fun <- function(processingSteps,
	domain = NULL, put = NULL)
{
	function(X, ..., MoreArgs)
	{
		browser()
		nindex <- ...length()
		index <- ..1
		if ( nindex > 1L ) {
			index2 <- ..2
		} else {
			index2 <- NULL
		}
		cid <- attr(X, "chunkid")
		ans <- vector("list", length(X))
		for ( i in seq_along(X) )
		{
			if ( is.matrix(X) ) {
				continuous <- TRUE
				xi <- xj <- X[,i]
				t1 <- index
				t2 <- index2
			} else if ( is.list(X) ) {
				continuous <- FALSE
				xi <- xj <- X[[i]]
				t1 <- index[[i]]
				t2 <- index2[[i]]
			} else {
				stop("unexpected chunk class: ", sQuote(class(X)[1L]))
			}
			nxi <- length(xi)
			ncout <- if (continuous) 2L else nindex + 1L
			for ( j in seq_along(processingSteps) )
			{
				psj <- processingSteps[[j]]
				if ( nindex == 1L ) {
					xj <- executeProcessingStep(psj, xi, t1)
				} else if ( nindex == 2L ) {
					xj <- executeProcessingStep(psj, xi, t1, t2)
				} else {
					stop("more than 2 'index' arrays not allowed")
				}
				if ( length(xj) == nxi ) {
					xi <- xj
				} else if ( is.matrix(xj) ) {
					if ( ncol(xj) == ncout ) {
						if ( ncol(xj) == 2L ) {
							t1 <- xj[,1L]
							xi <- xj[,2L]
						} else if ( ncol(xj) == 3L ) {
							t1 <- xj[,1L]
							t2 <- xj[,2L]
							xi <- xj[,3L]
						}
					} else {
						stop("expected ", ncout,
							" columns in output but received ", ncol(xj))
					}
				} else {
					stop("expected ", nxi,
						" elements in output but received ", length(xj))
				}
			}
			if ( isTRUE(all.equal(t1, domain, check.attributes=FALSE)) ) {
				ans[[i]] <- xi
			} else if ( is.matrix(xj) || (!is.null(put) && !continuous) ) {
				ans[[i]] <- switch(ncout - 1L,
					`1`=list(t1, xi),
					`2`=list(t1, t2, xi))
			} else if ( length(xi) == nxi ) {
				ans[[i]] <- xi
			} else {
				stop("length of output [", length(xi), "] ",
					"does not match input [", nxi, "]")
			}
		}
		if ( cid == 1L && !is.null(put) && !is.list(ans[[1L]]) )
		{
			if ( !is.null(domain) ) {
				put(as.double(domain))
			} else if ( continuous ) {
				put(as.double(index))
			}
		}
		if ( is.list(ans[[1L]]) )
			ans <- do.call(c, ans)
		if ( is.null(put) ) {
			ans
		} else {
			put(ans, cid)
		}
	}
}

