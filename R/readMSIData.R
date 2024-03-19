
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

readImzML <- function(file, memory = FALSE,
	mass.range = NULL, resolution = NA, units = c("ppm", "mz"),
	guess.max = 1000L, as = "auto", parse.only=FALSE,
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
		if ( getCardinalVerbose() )
			message("detected bundle directory: ", sQuote(path))
		path <- file.path(path, basename(path))
	}
	if ( getCardinalVerbose() )
		message("parsing imzML file: ", sQuote(path))
	ans <- .read_imzML(path, parse.only=parse.only)
	if ( getCardinalVerbose() ) {
		if ( isTRUE(ans@continuous) )
			message("detected 'continuous' imzML")
		if ( isFALSE(ans@continuous) )
			message("detected 'processed' imzML")
		if ( isTRUE(ans@centroided) )
			message("detected 'centroided' spectra")
		if ( isFALSE(ans@centroided) )
			message("detected 'profile' spectra")
	}
	if ( parse.only )
		return(ans)
	ans <- .read_featureData(ans, path)
	ans <- .read_pixelData(ans, path)
	as <- match.arg(as, c("auto", "MSImagingExperiment", "MSImagingArrays"))
	if ( as == "MSImagingExperiment" || 
		(as == "auto" && isTRUE(ans@continuous)) ||
		(!is.null(mass.range) || !is.na(resolution)) )
	{
		if ( getCardinalVerbose() )
			message("creating MSImagingExperiment")
		ans <- convertMSImagingArrays2Experiment(ans,
			mass.range=mass.range, resolution=resolution,
			units=units, guess.max=guess.max, BPPARAM=BPPARAM)
	}
	if ( as == "MSImagingArrays" )
		ans <- convertMSImagingExperiment2Arrays(ans)
	if ( memory ) {
		if ( getCardinalVerbose() )
			message("loading spectra into memory")
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
	if ( getCardinalVerbose() ) {
		message("returning ", class(ans))
		message("done.")
	}
	ans
}

.read_imzML <- function(path, parse.only = FALSE)
{
	parse <- CardinalIO::parseImzML(path, ibd=TRUE)
	if ( parse.only )
		return(parse)
	mz <- parse[["ibd"]][["mz"]]
	intensity <- parse[["ibd"]][["intensity"]]
	positions <- parse[["run"]][["spectrumList"]][["positions"]]
	if ( length(unique(positions[["position z"]])) > 1L ) {
		coord <- data.frame(
			x=as.numeric(positions[["position x"]]),
			y=as.numeric(positions[["position y"]]),
			z=as.numeric(positions[["position z"]]))
	} else {
		coord <- data.frame(
			x=as.numeric(positions[["position x"]]),
			y=as.numeric(positions[["position y"]]))
	}
	run <- basename(tools::file_path_sans_ext(path))
	spectraData <- SpectraArrays(list(mz=mz, intensity=intensity))
	pixelData <- PositionDataFrame(coord=coord, run=run)
	fileContent <- parse[["fileDescription"]][["fileContent"]]
	if ( "IMS:1000030" %in% names(fileContent)) {
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
	MSImagingArrays(spectraData, pixelData=pixelData,
		experimentData=as(parse, "ImzMeta"),
		centroided=centroided,
		continuous=continuous)
}

.read_pixelData <- function(object, path)
{
	path <- paste0(tools::file_path_sans_ext(path), ".pdata")
	if ( file.exists(path) ) {
		if ( getCardinalVerbose() )
			message("detected file: ", sQuote(basename(path)))
		pdata <- read.table(path)
		run <- pdata[["run"]]
		if ( is.null(run) )
			stop("'run' missing from spectra metadata")
		pdata[["run"]] <- factor(run)
		pixelData(object)[names(pdata)] <- pdata
	}
	object
}

.read_featureData <- function(object, path)
{
	path <- paste0(tools::file_path_sans_ext(path), ".fdata")
	if ( file.exists(path) ) {
		if ( getCardinalVerbose() )
			message("detected file: ", sQuote(basename(path)))
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



#### Read Analyze 7.5 file(s) ####
## -------------------------------

readAnalyze <- function(file, memory = FALSE, as = "auto", ...)
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
	if ( getCardinalVerbose() )
		message("parsing Analyze file: '", path, "'")
	ans <- .read_Analyze(path)
	as <- match.arg(as, c("auto", "MSImagingExperiment", "MSImagingArrays"))
	if ( as == "MSImagingExperiment" || as == "auto" )
	{
		if ( getCardinalVerbose() )
			message("creating MSImagingExperiment")
		ans <- convertMSImagingArrays2Experiment(ans)
	}
	if ( memory ) {
		if ( getCardinalVerbose() )
			message("loading spectra into memory")
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
	if ( getCardinalVerbose() ) {
		message("returning ", class(ans))
		message("done.")
	}
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

