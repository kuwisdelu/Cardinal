
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
		stop("can't recognize file extension: ", ext)
	}
}


#### Write imzML file(s) ####
## --------------------------

setMethod("writeImzML", "MSImagingExperiment_OR_Arrays", 
	function(object, file, bundle = TRUE, ...)
	{
		path <- normalizePath(file, mustWork=FALSE)
		if ( bundle ) {
			if ( ok <- dir.exists(path) ) {
				if ( getCardinalVerbose() )
					message("using bundle directory: ", sQuote(path))
			} else {
				if ( getCardinalVerbose() )
					message("creating bundle directory: ", sQuote(path))
				ok <- dir.create(path)
				if ( !ok )
					stop("failed to create bundle directory")
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
		ok <- .write_MSImagingExperiment_OR_Arrays(object, path=path, ...)
		if ( getCardinalVerbose() ) {
			if ( bundle )
				message("output bundle directory: ", sQuote(dirname(path)))
			message("done.")
		}
		invisible(ok)
	})

.write_MSImagingExperiment_OR_Arrays <- function(object, path,
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
	if ( getCardinalVerbose() )
		message("writing ", sQuote(format), " imzML file: ", sQuote(path))
	ok <- CardinalIO::writeImzML(experimentData(object), file=path,
		positions=positions, mz=mz, intensity=intensity, ...)
	if ( ok ) {
		if ( getCardinalVerbose() ) {
			outpath <- attr(ok, "outpath")
			message("wrote file: ", sQuote(basename(outpath[1L])))
			message("wrote file: ", sQuote(basename(outpath[2L])))
		}
	} else {
		stop("failed to write imzML")
	}
	.write_featureData(object, path)
	.write_pixelData(object, path)
	invisible(ok)
}

.write_pixelData <- function(object, path)
{
	path <- paste0(tools::file_path_sans_ext(path), ".pdata")
	vars <- setdiff(spectraVariables(object), coordNames(object))
	if ( nrun(object) > 1L || length(vars) > 1L ) {
		if ( file.exists(path) )
			warning("file ", sQuote(path), " already exists and will be overwritten")
		write.table(pixelData(object)[vars], file=path)
		if ( getCardinalVerbose() )
			message("wrote file: ", sQuote(basename(path)))
	}
}

.write_featureData <- function(object, path)
{
	path <- paste0(tools::file_path_sans_ext(path), ".fdata")
	if ( is(object, "MSImagingExperiment") && length(featureData(object)) > 1L ) {
		if ( file.exists(path) )
			warning("file ", sQuote(path), " already exists and will be overwritten")
		write.table(featureData(object), file=path)
		if ( getCardinalVerbose() )
			message("wrote file: ", sQuote(basename(path)))
	}
}


#### Write Analyze 7.5 file(s) ####
## --------------------------------

writeAnalyze <- function(object, file, ...)
{
	path <- normalizePath(file, mustWork=FALSE)
	if ( !is(object, "MSImagingExperiment") )
		object <- convertMSImagingExperiment2Arrays(object)
	if ( getCardinalVerbose() )
		message("writing Analyze 7.5 file: ", sQuote(file))
	ok <- CardinalIO::writeAnalyze(spectra(object), mz=mz(object), file=file, ...)
	if ( ok ) {
		if ( getCardinalVerbose() ) {
			outpath <- attr(ok, "outpath")
			message("wrote file: ", sQuote(basename(outpath[1L])))
			message("wrote file: ", sQuote(basename(outpath[2L])))
			message("wrote file: ", sQuote(basename(outpath[3L])))
		}
	} else {
		stop("failed to write Analyze 7.5")
	}
}

