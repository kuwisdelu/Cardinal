
#### Write MSI data file(s) ####
## -----------------------------

writeMSIData <- function(object, file, ...)
{
	ext <- tools::file_ext(file)
	if ( tolower(ext) %in% c("ibd", "imzml") ) {
		writeImzML(object, file, ...)
	} else if ( tolower(ext) %in% c("img", "hdr", "t2m") ) {
		writeAnalyze(object, file, ...)
	} else {
		.Error("can't recognize file extension: ", ext)
	}
}


#### Write imzML file(s) ####
## --------------------------

setMethod("writeImzML", "MSImagingExperiment_OR_Arrays", 
	function(object, file, bundle = TRUE,
		verbose = getCardinalVerbose(), ...)
	{
		path <- normalizePath(file, mustWork=FALSE)
		if ( bundle ) {
			if ( ok <- dir.exists(path) ) {
				.Log("using bundle directory: ", sQuote(path),
					message=verbose)
			} else {
				.Log("creating bundle directory: ", sQuote(path),
					message=verbose)
				ok <- dir.create(path)
				if ( !ok )
					.Error("failed to create bundle directory")
			}
			path <- file.path(path, basename(path))
		}
		e <- experimentData(object)
		if ( is.null(e) )
			e <- CardinalIO::ImzMeta()
		if ( is.null(e$spectrumType) )
			e$spectrumType <- "MS1 spectrum"
		if ( is.null(e$spectrumRepresentation) ) {
			if ( isCentroided(object) ) {
				e$spectrumRepresentation <- "centroid spectrum"
			} else {
				e$spectrumRepresentation <- "profile spectrum"
			}
		}
		experimentData(object) <- e
		ok <- .write_imzML(object, path=path, verbose=verbose, ...)
		if ( bundle ) {
			.Log("output bundle directory: ", sQuote(dirname(path)),
				message=verbose)
		}
		.Log("done.",
			message=verbose)
		invisible(ok)
	})

.write_imzML <- function(object, path, verbose,
	positions = NULL, mz = NULL, intensity = NULL, ...)
{
	if ( is.null(positions) )
		positions <- coord(object)
	if ( is.null(mz) )
		mz <- mz(object)
	if ( is.null(intensity) )
		intensity <- intensity(object)
	if ( is.numeric(mz) || is(mz, "matter_vec") ) {
		format <- "continuous"
	} else {
		format <- "processed"
	}
	.Log("writing ", sQuote(format), " imzML file: ", sQuote(path),
		message=verbose)
	ok <- CardinalIO::writeImzML(experimentData(object), file=path,
		positions=positions, mz=mz, intensity=intensity, ...)
	if ( ok ) {
		outpath <- attr(ok, "outpath")
		.Log("wrote file: ", sQuote(basename(outpath[1L])),
			message=verbose)
		.Log("wrote file: ", sQuote(basename(outpath[2L])),
			message=verbose)
	} else {
		.Error("failed to write imzML")
	}
	.write_featureData(object, path, verbose)
	.write_pixelData(object, path, verbose)
	invisible(ok)
}

.write_pixelData <- function(object, path, verbose)
{
	path <- paste0(tools::file_path_sans_ext(path), ".pdata")
	vars <- setdiff(spectraVariables(object), coordNames(object))
	if ( nrun(object) > 1L || length(vars) > 1L ) {
		if ( file.exists(path) )
			.Warn("file ", sQuote(path), " already exists and will be overwritten")
		write.table(pixelData(object)[vars], file=path)
		.Log("wrote file: ", sQuote(basename(path)),
			message=verbose)
	}
}

.write_featureData <- function(object, path, verbose)
{
	path <- paste0(tools::file_path_sans_ext(path), ".fdata")
	if ( is(object, "MSImagingExperiment") && length(featureData(object)) > 1L ) {
		if ( file.exists(path) )
			.Warn("file ", sQuote(path), " already exists and will be overwritten")
		write.table(featureData(object), file=path)
		.Log("wrote file: ", sQuote(basename(path)),
			message=verbose)
	}
}


#### Write Analyze 7.5 file(s) ####
## --------------------------------

setMethod("writeAnalyze", "MSImagingExperiment", 
	function(object, file, verbose = getCardinalVerbose(), ...)
	{
		.write_Analyze(spectra(object), file=file, verbose=verbose,
			positions=coord(object), domain=mz(object), ...)
	})

setMethod("writeAnalyze", "SpectralImagingExperiment", 
	function(object, file, verbose = getCardinalVerbose(), ...)
	{
		.write_Analyze(spectra(object), file=file, verbose=verbose,
			positions=coord(object), ...)
	})

.write_Analyze <- function(x, file, verbose, positions, domain, ...)
{
	path <- normalizePath(file, mustWork=FALSE)
	.Log("writing Analyze 7.5 file: ", sQuote(file),
		message=verbose)
	ok <- CardinalIO::writeAnalyze(x, file=file,
		positions=positions, domain=domain, ...)
	if ( ok ) {
		outpath <- attr(ok, "outpath")
		.Log("wrote file: ", sQuote(basename(outpath[1L])),
			message=verbose)
		.Log("wrote file: ", sQuote(basename(outpath[2L])),
			message=verbose)
		.Log("wrote file: ", sQuote(basename(outpath[3L])),
			message=verbose)
	} else {
		.Error("failed to write Analyze 7.5")
	}
}

