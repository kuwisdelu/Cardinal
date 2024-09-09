
#### Spectral pre-processing ####
## ------------------------------

setMethod("addProcessing", "SpectralImagingData",
	function(object, FUN, label, metadata = list(),
		verbose = getCardinalVerbose(), ...)
{
	args <- list(...)
	step <- ProcessingStep(FUN, ARGS=args)
	step <- setNames(list(step), label)
	metadata <- setNames(list(c(metadata, args)), label)
	ps <- processingData(object)
	psnew <- c(ps, step)
	psnew <- structure(psnew,
		queued=c(attr(ps, "queued"), metadata),
		processed=attr(ps, "processed"))
	processingData(object) <- psnew
	.Log("queued: ", label, message=verbose)
	if ( validObject(object) )
		object
})

reset <- function(object, ...)
{
	ps <- processingData(object)
	ps <- structure(list(), processed=attr(ps, "processed"))
	processingData(object) <- ps
	object
}

.processing_id <- function() {
	paste0("processing_", format(Sys.time(), format="%Y%m%d%H%M%S"))
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
		if ( length(index) > 1L )
			.Error("file output with more than 1 'index' array not allowed ",
				"for class ", sQuote(class(object)[1L]))
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
		mz <- spectra(object, indexname)
		intensity <- spectra(object, spectraname)
		outfile <- paste0(tools::file_path_sans_ext(outfile), ".imzML")
		ok <- writeImzML(object, file=outfile, mz=mz, intensity=intensity, asis=TRUE)
		newpath <- attr(ok, "outpath")[2L]
		path(spectra(object, indexname)) <- newpath
		path(spectra(object, spectraname)) <- newpath
	}
	object
}


# SpectralImagingExperiment

setMethod("process", "SpectralImagingExperiment",
	function(object, spectra = "intensity", index = NULL,
		domain = NULL, outfile = NULL,
		verbose = getCardinalVerbose(), chunkopts = list(),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( length(processingData(object)) == 0L )
		return(object)
	ps <- processingData(object)
	.Log("processing: ", paste0(names(ps), collapse=", "),
		message=verbose)
	if ( !is.null(outfile) ) {
		outfile <- normalizePath(outfile, mustWork=FALSE)
		pid <- ipcid()
		put <- matter::chunk_writer(pid, outfile)
		.Log("writing output to path = ", sQuote(outfile),
			message=verbose)
	} else {
		put <- NULL
	}
	if ( length(index) > 1L )
		.Error("more than 1 'index' array not allowed ",
			"for class ", sQuote(class(object)[1L]))
	snm <- spectra
	inm <- index
	spectra <- spectra(object, snm)
	if ( is.null(spectra) )
		.Error("spectra ", sQuote(snm), " not found")
	if ( is.null(inm) ) {
		inm <- "index"
		index <- seq_len(nrow(object))
	} else {
		index <- featureData(object)[[inm]]
		if ( is.null(index) )
			.Error("index ", sQuote(inm), " not found")
	}
	FUN <- .process_fun(ps, domain=domain, put=put)
	ans <- chunk_colapply(spectra, FUN, index,
			verbose=verbose, chunkopts=chunkopts,
			BPPARAM=BPPARAM)
	object <- .postprocess_SpectralImagingExperiment(ans,
		object=object, domain=domain, spectraname=snm, indexname=inm)
	metadata(object)[[.processing_id()]] <- attr(ps, "queued")
	processingData(object) <- structure(list(),
		processed=c(attr(ps, "processed"), attr(ps, "queued")))
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
			index <- featureData(object)[[indexname]]
		}
	} else {
		if ( is.null(domain) )
			domain <- featureData(object)[[indexname]]
		if ( length(ans) == 2L * length(object) ) {
			spectra <- ans[seq(2L, length(ans), by=2L),drop=drop]
			index <- ans[seq(1L, length(ans) - 1L, by=2L),drop=drop]
		} else {
			.Error("processed length [", length(ans), "] ",
				"does not match length of object [", length(object), "]")
		}
		spectra <- sparse_mat(index=index,
			data=spectra, domain=domain,
			nrow=nrow(object), ncol=ncol(object),
			tolerance=estres(domain),
			sampler="none")
	}
	.Log("output spectra: ", spectraname,
		message=getCardinalVerbose())
	if ( is.null(domain) || identical(domain, index) ) {
		spectra(object, spectraname) <- spectra
	} else {
		spectraData <- SpectraArrays(setNames(list(spectra), spectraname))
		featureData <- DataFrame(setNames(list(domain), indexname))
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
		verbose = getCardinalVerbose(), chunkopts = list(),
		BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( length(processingData(object)) == 0L )
		return(object)
	ps <- processingData(object)
	.Log("processing: ", paste0(names(ps), collapse=", "),
		message=verbose)
	if ( !is.null(outfile) ) {
		outfile <- normalizePath(outfile, mustWork=FALSE)
		pid <- ipcid()
		put <- matter::chunk_writer(pid, outfile)
		.Log("writing output to path = ", sQuote(outfile),
			message=verbose)
	} else {
		put <- NULL
	}
	if ( length(index) > 3L )
		.Error("more than 3 'index' arrays not allowed ",
			"for class ", sQuote(class(object)[1L]))
	nindex <- max(1L, length(index))
	snm <- spectra
	inm <- index
	spectra <- spectra(object, snm)
	if ( is.null(spectra) )
		.Error("spectra ", sQuote(snm), " not found")
	if ( is.null(inm) ) {
		inm <- "index"
		index <- lapply(lengths(spectra), seq_len)
	} else {
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
			if ( is.null(index3) )
				.Error("index ", sQuote(inm[3L]), " not found")
		}
	}
	FUN <- .process_fun(ps, domain=domain, put=put)
	if ( nindex == 1L ) {
		ans <- chunk_mapply(FUN, spectra, index,
			verbose=verbose, chunkopts=chunkopts,
			BPPARAM=BPPARAM)
	} else if ( nindex == 2L ) {
		ans <- chunk_mapply(FUN, spectra, index, index2,
			verbose=verbose, chunkopts=chunkopts,
			BPPARAM=BPPARAM)
	} else if ( nindex == 3L ) {
		ans <- chunk_mapply(FUN, spectra, index, index2, index3,
			verbose=verbose, chunkopts=chunkopts,
			BPPARAM=BPPARAM)
	} else {
		.Error("too many 'index' arrays")
	}
	object <- .postprocess_SpectralImagingArrays(ans,
		object=object, domain=domain, spectraname=snm, indexname=inm)
	metadata(object)[[.processing_id()]] <- attr(ps, "queued")
	processingData(object) <- structure(list(),
		processed=c(attr(ps, "processed"), attr(ps, "queued")))
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
		index3 <- NULL
	} else if ( length(ans) == 2L * length(object) ) {
		spectra <- ans[seq(2L, length(ans), by=2L),drop=drop]
		index <- ans[seq(1L, length(ans) - 1L, by=2L),drop=drop]
		index2 <- NULL
		index3 <- NULL
	} else if ( length(ans) == 3L * length(object) ) {
		spectra <- ans[seq(3L, length(ans), by=3L),drop=drop]
		index <- ans[seq(1L, length(ans) - 2L, by=3L),drop=drop]
		index2 <- ans[seq(2L, length(ans) - 1L, by=3L),drop=drop]
		index3 <- NULL
	} else if ( length(ans) == 4L * length(object) ) {
		spectra <- ans[seq(4L, length(ans), by=4L),drop=drop]
		index <- ans[seq(1L, length(ans) - 3L, by=4L),drop=drop]
		index2 <- ans[seq(2L, length(ans) - 2L, by=4L),drop=drop]
		index3 <- ans[seq(3L, length(ans) - 1L, by=4L),drop=drop]
	} else {
		.Error("processed length [", length(ans), "] ",
			"does not match length of object [", length(object), "]")
	}
	if ( !is.null(index) ) {
		.Log("output index: ", indexname[1L],
			message=getCardinalVerbose())
		spectra(object, indexname[1L]) <- index
	}
	if ( !is.null(index2) ) {
		.Log("output index: ", indexname[2L],
			message=getCardinalVerbose())
		spectra(object, indexname[2L]) <- index2
	}
	if ( !is.null(index3) ) {
		.Log("output index: ", indexname[3L],
			message=getCardinalVerbose())
		spectra(object, indexname[3L]) <- index2
	}
	.Log("output spectra: ", spectraname,
		message=getCardinalVerbose())
	spectra(object, spectraname) <- spectra
	if ( validObject(object) )
		object
}

# process worker function

.process_fun <- function(processingSteps,
	domain = NULL, put = NULL)
{
	isoclos(function(X, ..., MoreArgs)
	{
		nindex <- ...length()
		index <- ..1
		if ( nindex >= 2L ) {
			index2 <- ..2
		} else {
			index2 <- NULL
		}
		if ( nindex >= 3L ) {
			index3 <- ..3
		} else {
			index3 <- NULL
		}
		if ( is.matrix(X) ) {
			continuous <- TRUE
			N <- ncol(X)
		} else if ( is.list(X) ) {
			continuous <- FALSE
			N <- length(X)
		} else {
			.Error("unexpected chunk class: ", sQuote(class(X)[1L]))
		}
		cid <- attr(X, "chunkid")
		ans <- vector("list", N)
		for ( i in seq_len(N) )
		{
			if ( continuous ) {
				xi <- xj <- X[,i]
				t1 <- index
				t2 <- index2
				t3 <- index3
			} else {
				xi <- xj <- X[[i]]
				t1 <- index[[i]]
				t2 <- index2[[i]]
				t3 <- index3[[i]]
			}
			nxi <- length(xi)
			nci <- if (continuous) 2L else nindex + 1L
			for ( j in seq_along(processingSteps) )
			{
				psj <- processingSteps[[j]]
				if ( nindex == 1L ) {
					xj <- executeProcessingStep(psj, xi, t1)
				} else if ( nindex == 2L ) {
					xj <- executeProcessingStep(psj, xi, t1, t2)
				} else if ( nindex == 3L ) {
					xj <- executeProcessingStep(psj, xi, t1, t2, t3)
				} else {
					.Error("more than 3 'index' arrays not allowed")
				}
				if ( length(xj) == nxi ) {
					xi <- xj
				} else if ( is.matrix(xj) ) {
					if ( ncol(xj) == nci ) {
						if ( ncol(xj) == 2L ) {
							t1 <- xj[,1L]
							xi <- xj[,2L]
						} else if ( ncol(xj) == 3L ) {
							t1 <- xj[,1L]
							t2 <- xj[,2L]
							xi <- xj[,3L]
						} else if ( ncol(xj) == 4L ) {
							t1 <- xj[,1L]
							t2 <- xj[,2L]
							t3 <- xj[,3L]
							xi <- xj[,4L]
						}						
					} else {
						.Error("expected ", nci,
							" columns in output but received ", ncol(xj))
					}
				} else {
					.Error("expected ", nxi,
						" elements in output but received ", length(xj))
				}
			}
			if ( isTRUE(all.equal(t1, domain, check.attributes=FALSE)) ) {
				ans[[i]] <- xi
			} else if ( is.matrix(xj) || (!is.null(put) && !continuous) ) {
				ans[[i]] <- switch(nci - 1L,
					`1`=list(t1, xi),
					`2`=list(t1, t2, xi),
					`3`=list(t1, t2, t3, xi))
			} else if ( length(xi) == nxi ) {
				ans[[i]] <- xi
			} else {
				.Error("length of output [", length(xi), "] ",
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
	}, CardinalEnv())
}

