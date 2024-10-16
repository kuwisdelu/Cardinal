
#### Read MS imaging data file(s) ####
## -----------------------------------

readMSIData <- function(file, ...)
{
	ext <- tools::file_ext(file)
	if ( tolower(ext) %in% c("ibd", "imzml") ) {
		readImzML(file, ...)
	} else if ( tolower(ext) %in% c("img", "hdr", "t2m") ) {
		readAnalyze(file, ...)
	} else {
		stop("can't recognize file extension: ", ext)
	}
}


#### Read imzML file(s) ####
## -------------------------

readImzML <- function(file, memory = FALSE, check = FALSE,
	mass.range = NULL, resolution = NA, units = c("ppm", "mz"),
	guess.max = 1000L, as = "auto", parse.only = FALSE,
	verbose = getCardinalVerbose(), chunkopts = list(),
	BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( "name" %in% ...names() )
		.Deprecated(old="name", new="file")
	if ( "folder" %in% ...names() )
		.Deprecated(old="folder", new="file")
	if ( "attach.only" %in% ...names() ) {
		.Deprecated(old="attach.only", new="memory")
		memory <- !list(...)$attach.only
	}
	if ( tools::file_ext(file) != "" ) {
		path <- normalizePath(file, mustWork=TRUE)
	} else {
		dots <- list(...)
		if ( "folder" %in% names(dots) ) {
			path <- normalizePath(file.path(dots$folder, paste0(file, ".imzML")))
		} else {
			path <- normalizePath(paste0(file, ".imzML"))
		}
	}
	if ( isTRUE(file.info(path)$isdir) ) {
		.Log("detected bundle directory: ", sQuote(path),
			message=verbose)
		path <- file.path(path, basename(path))
	}
	.Log("parsing imzML file: ", sQuote(path),
		message=verbose)
	if ( file.size(path) > 1e9 ) {
		.Log("imzML file is large (>1GB) so this may take longer than usual",
			message=verbose)
	}
	ans <- .read_imzML(path, parse.only=parse.only, check=check, ...)
	if ( isTRUE(ans@continuous) ) {
		.Log("detected 'continuous' imzML",
			message=verbose)
	}
	if ( isFALSE(ans@continuous) ) {
		.Log("detected 'processed' imzML",
			message=verbose)
	}
	if ( isTRUE(ans@centroided) ) {
		.Log("detected 'centroided' spectra",
			message=verbose)
	}
	if ( isFALSE(ans@centroided) ) {
		.Log("detected 'profile' spectra",
			message=verbose)
	}
	is_dense <- isTRUE(ans@continuous)
	if ( parse.only )
		return(ans)
	ans <- .read_featureData(ans, path, verbose)
	ans <- .read_pixelData(ans, path, verbose)
	ans <- .read_metadata(ans, path, verbose)
	as <- match.arg(as, c("auto", "MSImagingExperiment", "MSImagingArrays"))
	if ( as == "MSImagingExperiment" || 
		(as == "auto" && is_dense) ||
		(!is.null(mass.range) || !is.na(resolution)) )
	{
		if ( missing(units) && !missing(resolution) )
			units <- get_units_from_names(resolution, units)
		.Log("creating MSImagingExperiment",
			message=verbose)
		ans <- convertMSImagingArrays2Experiment(ans,
			mass.range=mass.range, resolution=resolution,
			units=units, guess.max=guess.max,
			verbose=verbose, chunkopts=chunkopts,
			BPPARAM=BPPARAM)
	}
	if ( isCentroided(ans) && is(ans, "MSImagingArrays") )
	{
		.Log("NOTE: use peakAlign() to align centroided data",
			message=verbose)
	}
	if ( as == "MSImagingArrays" )
		ans <- convertMSImagingExperiment2Arrays(ans)
	if ( !isFALSE(memory) )
	{
		if ( memory == "shared" ) {
			.Log("fetching spectra into shared memory",
				message=verbose)
			ans <- fetch(ans,
				verbose=verbose, chunkopts=chunkopts,
				BPPARAM=BPPARAM)
		} else {
			.Log("loading spectra into memory",
				message=verbose)
			if ( is(ans, "MSImagingArrays") ) {
				for ( i in spectraNames(ans) )
					spectra(ans, i) <- as.list(spectra(ans, i))
			} else {
				if ( is.sparse(spectra(ans)) ) {
					atomindex(spectra(ans)) <- as.list(atomindex(spectra(ans)))
					atomdata(spectra(ans)) <- as.list(atomdata(spectra(ans)))
				} else {
					spectra(ans) <- as.matrix(spectra(ans))
				}
			}
		}
	}
	.Log("returning ", class(ans),
		message=verbose)
	ans
}

.read_imzML <- function(path, parse.only = FALSE,
	extra = NULL, extraArrays = NULL, check = FALSE)
{
	if ( is.null(extra) )
		extra <- c("3DPositionX", "3DPositionY", "3DPositionZ")
	parse <- CardinalIO::parseImzML(path, check=check,
		extra=extra, extraArrays=extraArrays, ibd=TRUE)
	if ( parse.only )
		return(parse)
	mz <- parse[["ibd"]][["mz"]]
	intensity <- parse[["ibd"]][["intensity"]]
	positions <- parse[["run"]][["spectrumList"]][["positions"]]
	extra <- parse[["run"]][["spectrumList"]][["extra"]]
	extra <- extra[vapply(extra, function(x) !all(is.na(x)), logical(1L))]
	extraArrays <- parse[["ibd"]][["extra"]]
	ids <- names(intensity)
	if ( length(unique(positions[["position z"]])) > 1L ) {
		coord <- data.frame(
			x=as.numeric(positions[["position x"]]),
			y=as.numeric(positions[["position y"]]),
			z=as.numeric(positions[["position z"]]))
	} else {
		coord <- data.frame(
			x=as.numeric(positions[["position x"]]),
			y=as.numeric(positions[["position y"]]))
		if ( "3DPositionZ" %in% names(extra) ) {
			coord$z <- as.numeric(extra[["3DPositionZ"]])
			coord$z <- match(coord$z, sort(unique(coord$z)))
		}
	}
	run <- basename(tools::file_path_sans_ext(path))
	spectraData <- SpectraArrays(list(intensity=intensity, mz=mz))
	pixelData <- PositionDataFrame(coord=coord, run=run, row.names=ids)
	if ( length(extra) > 0L )
		pixelData[names(extra)] <- extra
	if ( length(extraArrays) > 0L ) {
		for ( i in names(extraArrays) ) {
			if ( !is.null(extraArrays[[i]]) )
				spectraData[[i]] <- extraArrays[[i]]
		}
	}
	compression <- metadata(parse)[["compression"]]
	if ( !all(compression %in% "no compression") )
		.Warn("one or more binary data arrays use unsupported compression ",
			"schemes: ", paste0(sQuote(compression), collapse=", "))
	fileContent <- parse[["fileDescription"]][["fileContent"]]
	if ( "IMS:1000030" %in% names(fileContent) ) {
		continuous <- TRUE
	} else if ( "IMS:1000031" %in% names(fileContent)) {
		continuous <- FALSE
	} else {
		continuous <- NA
	}
	if ( "MS:1000127" %in% names(fileContent) ) {
		centroided <- TRUE
	} else if ( "MS:1000128" %in% names(fileContent) ) {
		centroided <- FALSE
	} else {
		centroided <- NA
	}
	experimentData <- try(as(parse, "ImzMeta"), silent=TRUE)
	if ( inherits(experimentData, "try-error") ) {
		.Warn("failed to convert experimental metadata:\n",
			attr(experimentData, "condition")$message)
		experimentData <- NULL
	}
	MSImagingArrays(spectraData, pixelData=pixelData,
		experimentData=experimentData,
		centroided=centroided,
		continuous=continuous)
}

.read_pixelData <- function(object, path, verbose)
{
	path <- paste0(tools::file_path_sans_ext(path), ".pdata")
	if ( file.exists(path) ) {
		.Log("detected metadata file: ", sQuote(basename(path)),
			message=verbose)
		pdata <- read.table(path)
		run <- pdata[["run"]]
		if ( is.null(run) )
			stop("'run' missing from spectra metadata")
		pdata[["run"]] <- factor(run)
		pixelData(object)[names(pdata)] <- pdata
	}
	object
}

.read_featureData <- function(object, path, verbose)
{
	path <- paste0(tools::file_path_sans_ext(path), ".fdata")
	if ( file.exists(path) ) {
		.Log("detected metadata file: ", sQuote(basename(path)),
			message=verbose)
		fdata <- read.table(path)
		if ( is(object, "MSImagingArrays") ) {
			mz <- fdata[["mz"]]
			if ( is.null(mz) )
				stop("'mz' missing from feature metadata")
			spectra <- sparse_mat(index=mz(object),
				data=intensity(object), domain=mz,
				nrow=length(mz), ncol=length(object),
				tolerance=0.5 * estres(mz),
				sampler="max")
			object <- MSImagingExperiment(spectra,
				featureData=MassDataFrame(mz=mz),
				pixelData=pixelData(object),
				experimentData=experimentData(object),
				metadata=metadata(object),
				centroided=centroided(object))
		}
		featureData(object)[names(fdata)] <- fdata
	}
	object
}

.read_metadata <- function(object, path, verbose)
{
	path <- paste0(tools::file_path_sans_ext(path), ".metadata")
	if ( file.exists(path) ) {
		.Log("detected metadata file: ", sQuote(basename(path)),
			message=verbose)
		metadata <- try(dget(path))
		if ( inherits(metadata, "try-error") || !is.list(metadata) ) {
			.Warn("failed to read metadata")
		} else {
			metadata(object) <- metadata
		}
	}
	object
}


#### Read Analyze 7.5 file(s) ####
## -------------------------------

readAnalyze <- function(file, memory = FALSE, as = "auto",
	verbose = getCardinalVerbose(), chunkopts = list(),
	BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( "name" %in% ...names() )
		.Deprecated(old="name", new="file")
	if ( "folder" %in% ...names() )
		.Deprecated(old="folder", new="file")
	if ( "attach.only" %in% ...names() ) {
		.Deprecated(old="attach.only", new="memory")
		memory <- !list(...)$attach.only
	}
	if ( tools::file_ext(file) != "" ) {
		path <- normalizePath(file, mustWork=TRUE)
	} else {
		dots <- list(...)
		if ( "folder" %in% names(dots) ) {
			path <- normalizePath(file.path(dots$folder, paste0(file, ".imzML")))
		} else {
			path <- normalizePath(paste0(file, ".imzML"))
		}
	}
	.Log("parsing Analyze file: '", path, "'",
		message=verbose)
	ans <- .read_Analyze(path)
	as <- match.arg(as, c("auto", "MSImagingExperiment", "MSImagingArrays"))
	if ( as == "MSImagingExperiment" || as == "auto" )
	{
		.Log("creating MSImagingExperiment",
			message=verbose)
		ans <- convertMSImagingArrays2Experiment(ans)
	}
	if ( !isFALSE(memory) )
	{
		if ( memory == "shared" ) {
			.Log("fetching spectra into shared memory",
				message=verbose)
			ans <- fetch(ans,
				verbose=verbose, chunkopts=chunkopts,
				BPPARAM=BPPARAM)
		} else {
			.Log("loading spectra into memory",
				message=verbose)
			if ( is(ans, "MSImagingArrays") ) {
				mz(ans) <- as.list(mz(ans))
				intensity(ans) <- as.list(intensity(ans))
			} else {
				if ( is.sparse(spectra(ans)) ) {
					atomindex(spectra(ans)) <- as.list(atomindex(spectra(ans)))
					atomdata(spectra(ans)) <- as.list(atomdata(spectra(ans)))
				} else {
					spectra(ans) <- as.matrix(spectra(ans))
				}
			}
		}
	}
	.Log("returning ", class(ans),
		message=verbose)
	ans
}

.read_Analyze <- function(path)
{
	parse <- CardinalIO::parseAnalyze(path)
	dms <- dim(parse$img)
	mz <- matter_vec(
		type=type(atomdata(parse$t2m)),
		path=path(atomdata(parse$t2m)),
		length=dms[1L])
	spectra <- matter_mat(
		type=type(atomdata(parse$img)),
		path=path(atomdata(parse$img)),
		nrow=dms[1L], ncol=prod(dms[-1L]))
	if ( length(dms) > 3L && dms[4L] != 1L ) {
		coord <- expand.grid(
			x=seq_len(dms[2L]),
			y=seq_len(dms[3L]),
			z=seq_len(dms[4L]))
	} else {
		coord <- expand.grid(
			x=seq_len(dms[2L]),
			y=seq_len(dms[3L]))
	}
	run <- basename(tools::file_path_sans_ext(path))
	featureData <- MassDataFrame(mz=as.vector(mz))
	pixelData <- PositionDataFrame(coord=coord, run=run)
	MSImagingExperiment(spectra,
		featureData=featureData,
		pixelData=pixelData)
}

