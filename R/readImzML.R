
#### Read imzML files ####
## ----------------------

readImzML <- function(name, folder = getwd(), attach.only = TRUE,
	mass.range = NULL, resolution = NA, units = c("ppm", "mz"),
	guess.max = 1000L, as = "MSImagingExperiment", parse.only=FALSE,
	BPPARAM = getCardinalBPPARAM(), ...)
{
	# get output format
	outclass <- match.arg(as)
	# check for files
	xmlpath <- normalizePath(file.path(folder, paste(name, ".imzML", sep="")),
		mustWork=FALSE)
	if ( !file.exists(xmlpath) ) .stop("expected file ", xmlpath, " does not exist")
	ibdpath <- normalizePath(file.path(folder, paste(name, ".ibd", sep="")),
		mustWork=FALSE)
	if ( !file.exists(ibdpath) ) .stop("expected file ", ibdpath, " does not exist")
	# read imzML file
	.message("reading imzML file: '", xmlpath, "'")
	parse <- CardinalIO::parseImzML(xmlpath, ibd=TRUE)
	fileContent <- parse[["fileDescription"]][["fileContent"]]
	# ibd binary type
	if ( "IMS:1000030" %in% names(fileContent)) {
		ibdbinarytype <- "continuous"
	} else if ( "IMS:1000031" %in% names(fileContent)) {
		ibdbinarytype <- "processed"
	} else {
		ibdbinarytype <- NULL
	}
	if ( !is.null(ibdbinarytype) )
		.message("detected ibd binary type: ", sQuote(ibdbinarytype))
	# spectrum representation
	if ( "MS:1000127" %in% names(fileContent)) {
		representation <- "centroid spectrum"
	} else if ( "MS:1000128" %in% names(fileContent)) {
		representation <- "profile spectrum"
	} else {
		representation <- NULL
	}
	if ( !is.null(representation) )
		.message("detected representation: ", sQuote(representation))
	# return parse
	if ( parse.only )
		return(parse)
	# read ibd file
	units <- match.arg(units)
	.message("reading ibd file: '", ibdpath, "'")
	object <- .attachIbd(parse, name, ibdbinarytype, representation,
		attach.only, mass.range, resolution, units, guess.max)
	.log.collapse("loaded dataset:", capture.output(print(object)))
	if ( validObject(object) ) {
		.message("done.")
		object
	}
}

.attachIbd <- function(parse, name, ibdbinarytype, representation,
	attach.only, mass.range, resolution, units, guess.max)
{
	pmz <- parse[["ibd"]][["mz"]]
	pintensity <- parse[["ibd"]][["intensity"]]
	pcoord <- parse[["run"]][["spectrumList"]][["positions"]]
	if ( ibdbinarytype == "processed" )
	{
		if ( is.null(mass.range) || is.na(resolution) )
		{
			.message("auto-determining mass range and resolution...")
			mzmeta <- .detectMassRangeAndResolution(pmz,
				units=units, guess.max=guess.max)
			if ( is.null(mass.range) ) {
				mass.range <- mzmeta$mass.range
				.message("detected mass range: ",
					mass.range[1], " to ", mass.range[2])
			}
			if ( is.na(resolution) ) {
				resolution <- mzmeta$resolution
				.message("estimated mass resolution: ",
					resolution, " ", units)
			}
		}
		.message("using mass range: ", mass.range[1L], " to ", mass.range[2L])
		.message("using mass resolution: ", resolution, " ", units)
		mzmin <- mass.range[1]
		mzmax <- mass.range[2]
		if ( units == "ppm" ) {
			if ( floor(mzmin) <= 0 )
				.stop("m/z values must be positive for units='ppm'")
			mzout <- seq.ppm(
				from=floor(mzmin),
				to=ceiling(mzmax),
				ppm=resolution / 2) # seq.ppm == half-bin-widths
			tol <- c(relative = resolution * 1e-6)
		} else {
			mzout <- seq(
				from=floor(mzmin),
				to=ceiling(mzmax),
				by=resolution)  # seq == full-bin-widths
			tol <- c(absolute = resolution)
		}
		if ( "centroid spectrum" %in% representation)
		{
			.message("binning centroided peaks...")
			if ( is.finite(guess.max) ) {
				index <- floor(seq(from=1L, to=length(pmz), length.out=guess.max))
			} else {
				index <- seq_along(pmz)
			}
			peaks <- matter::binpeaks(pmz[index], domain=mzout)
			.message("number of binned peaks: ", length(peaks))
			mzout <- as.numeric(peaks)
		}
		spectra <- sparse_mat(index=pmz, data=pintensity,
			domain=mzout, nrow=length(mzout), ncol=length(pintensity),
			tolerance=tol, sampler="linear")
	} else {
		mzout <- pmz[[1L]]
		spectra <- as(pintensity, "matter_mat")
	}
	if ( length(unique(pcoord[["position z"]])) > 1L ) {
		coord <- data.frame(
			x=as.numeric(pcoord[["position x"]]),
			y=as.numeric(pcoord[["position y"]]),
			z=as.numeric(pcoord[["position z"]]))
	} else {
		coord <- data.frame(
			x=as.numeric(pcoord[["position x"]]),
			y=as.numeric(pcoord[["position y"]]))
	}
	if ( length(representation) > 0L ) {
		centroided <- representation == "centroid spectrum"
	} else {
		centroided <- NA
	}
	MSImagingExperiment(spectra,
		featureData=MassDataFrame(mz=mzout),
		pixelData=PositionDataFrame(coord=coord, run=name),
		metadata=list(parse=parse),
		centroided=centroided)
}

.detectMassRangeAndResolution <- function(pmz, units, guess.max)
{
	if ( units == "ppm" ) {
		fun <- function(m) {
			med <- median(1e6 * diff(m) / m[-1], na.rm=TRUE)
			lim <- range(m, na.rm=TRUE)
			c(med, lim)
		}
	} else {
		fun <- function(m) {
			med <- median(diff(m), na.rm=TRUE)
			lim <- range(m, na.rm=TRUE)
			c(med, lim)
		}
	}
	if ( is.finite(guess.max) ) {
		index <- floor(seq(from=1L, to=length(pmz), length.out=guess.max))
	} else {
		index <- seq_along(pmz)
	}
	mzmeta <- vapply(pmz[index], fun, numeric(3L))
	mzmeta <- as.matrix(mzmeta)
	mzmeta[!is.finite(mzmeta)] <- NA
	res <- median(mzmeta[1,], na.rm=TRUE)
	min <- round(min(mzmeta[2,], na.rm=TRUE), digits=4)
	max <- round(max(mzmeta[3,], na.rm=TRUE), digits=4)
	if ( units == "ppm" ) {
		res <- roundnear(res, precision=1)
	} else {
		res <- round(res, digits=4)
	}
	list(mass.range=c(min, max), resolution=res)
}

# .readImzML <- function(file) {
# 	parse <- .Call("C_readImzML", normalizePath(file), PACKAGE="Cardinal")
# 	len <- sapply(parse$experimentMetadata, nchar, type="bytes")
# 	experimentMetadata <- parse$experimentMetadata[len > 0]
# 	.MSImagingInfo(
# 		scanList=as(parse$scanList, "DataFrame"),
# 		mzArrayList=as(parse$mzArrayList, "DataFrame"),
# 		intensityArrayList=as(parse$intensityArrayList, "DataFrame"),
# 		metadata=experimentMetadata)
# }

# .readIbd <- function(file, info, outclass, attach.only,
# 	mass.range, resolution, units, BPPARAM)
# {
# 	file <- normalizePath(file)
# 	ibdtype <- metadata(info)[["ibd binary type"]]
# 	mz.ibdtype <- mzData(info)[["binary data type"]]
# 	intensity.ibdtype <- intensityData(info)[["binary data type"]]
# 	# read binary data
# 	if ( ibdtype == "continuous" ) {
# 		mz <- matter_vec(path=file,
# 			type=Ctypeof(mz.ibdtype[1]),
# 			offset=mzData(info)[["external offset"]][1],
# 			extent=mzData(info)[["external array length"]][1])
# 		intensity <- matter_mat(path=file,
# 			type=Ctypeof(intensity.ibdtype[1]),
# 			offset=intensityData(info)[["external offset"]],
# 			extent=intensityData(info)[["external array length"]])
# 		if ( attach.only ) {
# 			spectra <- intensity
# 		} else {
# 			spectra <- intensity[]
# 		}
# 		mz <- mz[]
# 	} else if ( ibdtype == "processed" ) {
# 		mz <- matter_list(path=file,
# 			type=Ctypeof(mz.ibdtype),
# 			offset=mzData(info)[["external offset"]],
# 			extent=mzData(info)[["external array length"]])
# 		intensity <- matter_list(path=file,
# 			type=Ctypeof(intensity.ibdtype),
# 			offset=intensityData(info)[["external offset"]],
# 			extent=intensityData(info)[["external array length"]])
# 		if ( is.null(mass.range) || is.na(resolution) ) {
# 			.message("auto-determining mass range and resolution...")
# 			mz.info <- .detectMassRangeAndResolution(mz, units=units, BPPARAM=BPPARAM)
# 			if ( is.null(mass.range) ) {
# 				mass.range <- mz.info$mass.range
# 				.message("detected mass range: ",
# 					mass.range[1], " to ", mass.range[2])
# 			}
# 			if ( is.na(resolution) ) {
# 				resolution <- mz.info$resolution
# 				.message("estimated mass resolution: ",
# 					resolution, " ", units)
# 			}
# 		}
# 		.message("using mass range: ", mass.range[1L], " to ", mass.range[2L])
# 		.message("using mass resolution: ", resolution, " ", units)
# 		mz.min <- mass.range[1]
# 		mz.max <- mass.range[2]
# 		if ( units == "ppm" ) {
# 			if ( floor(mz.min) <= 0 )
# 				.stop("m/z values must be positive for units='ppm'")
# 			mzout <- seq.ppm(
# 				from=floor(mz.min),
# 				to=ceiling(mz.max),
# 				ppm=resolution / 2) # seq.ppm == half-bin-widths
# 			error <- (resolution / 2) * 1e-6 * mzout
# 			tol <- c(relative = resolution * 1e-6)
# 		} else {
# 			mzout <- seq(
# 				from=floor(mz.min),
# 				to=ceiling(mz.max),
# 				by=resolution)  # seq == full-bin-widths
# 			error <- rep(resolution / 2, length(mzout))
# 			tol <- c(absolute = resolution)
# 		}
# 		mz.bins <- c(mzout[1] - error[1], mzout + error)
# 		if ( attach.only ) {
# 			data <- list(keys=mz, values=intensity)
# 			mz <- mzout
# 			spectra <- sparse_mat(index=data$keys, data=data$values,
# 				domain=mz, nrow=length(mz), ncol=length(intensity),
# 				tolerance=tol, sampler="linear")
# 		} else {
# 			if ( outclass == "MSImagingExperiment") {
# 				data <- list(keys=mz[], values=intensity[])
# 			}
# 			mz <- mzout
# 			spectra <- sparse_mat(index=data$keys, data=data$values,
# 				domain=mz, nrow=length(mz), ncol=length(intensity),
# 				tolerance=tol, sampler="linear")
# 		}
# 	}
# 	# set up coordinates
# 	x <- scans(info)[["position x"]]
# 	y <- scans(info)[["position y"]]
# 	z <- scans(info)[["position z"]]
# 	x3d <- scans(info)[["3DPositionX"]]
# 	y3d <- scans(info)[["3DPositionY"]]
# 	z3d <- scans(info)[["3DPositionZ"]]
# 	if ( allmissing(z) && allmissing(z3d) ) {
# 		coord <- data.frame(x=x, y=y)
# 	} else if ( allmissing(z3d) ) {
# 		coord <- data.frame(x=x, y=y, z=z)
# 	} else if ( nomissing(z3d) ) {
# 		z <- as.integer(xtfrm(round(z3d)))
# 		coord <- data.frame(x=x, y=y, z=z)
# 	} else {
# 		.stop("invalid pixel coordinates")
# 	}
# 	if ( !allmissing(x3d) && !allmissing(y3d) ) {
# 		if ( allmissing(z3d) ) {
# 			coordExact <- scans(info)[c("3DPositionX", "3DPositionY")]
# 		} else {
# 			coordExact <- scans(info)[c("3DPositionX", "3DPositionY", "3DPositionZ")]
# 		}
# 	} else {
# 		coordExact <- NULL
# 	}
# 	if ( outclass == "MSImagingExperiment" ) {
# 		object <- MSImagingExperiment(spectra,
# 			featureData=MassDataFrame(mz=mz),
# 			pixelData=PositionDataFrame(coord=coord,
# 				run=metadata(info)[["name"]]),
# 			metadata=metadata(info),
# 			centroided=isCentroided(info))
# 	} else {
# 		.stop("unrecognized outclass")
# 	}
# 	if ( !is.null(coordExact) )
# 		pixelData(object)[names(coordExact)] <- as.list(coordExact)
# 	object
# }



