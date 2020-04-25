
#### Read imzML files ####
## ----------------------

readImzML <- function(name, folder = getwd(), attach.only = TRUE,
	mass.range = NULL, resolution = NA, units = c("ppm", "mz"),
	as = "MSImagingExperiment", parse.only=FALSE,
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
	info <- .readImzML(xmlpath)
	# describe file
	ibdbinarytype <- metadata(info)[["ibd binary type"]]
	if ( !is.null(ibdbinarytype) )
		.message("detected ibd binary type: ", sQuote(ibdbinarytype))
	representation <- metadata(info)[["spectrum representation"]]
	if ( !is.null(representation) )
		.message("detected representation: ", sQuote(representation))
	# return parse
	if ( parse.only )
		return(info)
	# read ibd file
	metadata(info)[["files"]] <- c(xmlpath, ibdpath)
	metadata(info)[["name"]] <- name
	units <- match.arg(units)
	.message("reading ibd file: '", ibdpath, "'")
	object <- .readIbd(ibdpath, info, outclass=outclass, attach.only=attach.only,
		mass.range=mass.range, resolution=resolution, units=units, BPPARAM=BPPARAM)
	.log.collapse("loaded dataset:", capture.output(print(object)))
	if ( validObject(object) ) {
		.message("done.")
		object
	}
}

.readImzML <- function(file) {
	parse <- .Call("C_readImzML", normalizePath(file), PACKAGE="Cardinal")
	len <- sapply(parse$experimentMetadata, nchar, type="bytes")
	experimentMetadata <- parse$experimentMetadata[len > 0]
	.MSImagingInfo(
		scanList=as(parse$scanList, "DataFrame"),
		mzArrayList=as(parse$mzArrayList, "DataFrame"),
		intensityArrayList=as(parse$intensityArrayList, "DataFrame"),
		metadata=experimentMetadata)
}

.readIbd <- function(file, info, outclass, attach.only,
	mass.range, resolution, units, BPPARAM)
{
	file <- normalizePath(file)
	ibdtype <- metadata(info)[["ibd binary type"]]
	mz.ibdtype <- mzData(info)[["binary data type"]]
	intensity.ibdtype <- intensityData(info)[["binary data type"]]
	# read binary data
	if ( ibdtype == "continuous" ) {
		mz <- matter_vec(paths=file,
			datamode=Ctypeof(mz.ibdtype[1]),
			offset=mzData(info)[["external offset"]][1],
			extent=mzData(info)[["external array length"]][1])
		intensity <- matter_mat(paths=file,
			datamode=Ctypeof(intensity.ibdtype[1]),
			offset=intensityData(info)[["external offset"]],
			extent=intensityData(info)[["external array length"]])
		if ( attach.only ) {
			spectra <- intensity
		} else {
			spectra <- intensity[]
		}
		mz <- mz[]
	} else if ( ibdtype == "processed" ) {
		mz <- matter_list(paths=file,
			datamode=Ctypeof(mz.ibdtype),
			offset=mzData(info)[["external offset"]],
			extent=mzData(info)[["external array length"]])
		intensity <- matter_list(paths=file,
			datamode=Ctypeof(intensity.ibdtype),
			offset=intensityData(info)[["external offset"]],
			extent=intensityData(info)[["external array length"]])
		if ( is.null(mass.range) || is.na(resolution) ) {
			.message("auto-determining mass range and resolution...")
			mz.info <- .detectMassRangeAndResolution(mz, units=units, BPPARAM=BPPARAM)
			if ( is.null(mass.range) ) {
				mass.range <- mz.info$mass.range
				.message("detected mass range: ",
					mass.range[1], " to ", mass.range[2])
			}
			if ( is.na(resolution) ) {
				resolution <- mz.info$resolution
				.message("estimated mass resolution: ",
					resolution, " ", units)
			}
		}
		.message("using mass range: ", mass.range[1L], " to ", mass.range[2L])
		.message("using mass resolution: ", resolution, " ", units)
		mz.min <- mass.range[1]
		mz.max <- mass.range[2]
		if ( units == "ppm" ) {
			if ( floor(mz.min) <= 0 )
				.stop("m/z values must be positive for units='ppm'")
			mzout <- seq.ppm(
				from=floor(mz.min),
				to=ceiling(mz.max),
				ppm=resolution / 2) # seq.ppm == half-bin-widths
			error <- (resolution / 2) * 1e-6 * mzout
			tol <- c(relative = (resolution / 2) * 1e-6)
		} else {
			mzout <- seq(
				from=floor(mz.min),
				to=ceiling(mz.max),
				by=resolution)  # seq == full-bin-widths
			error <- rep(resolution / 2, length(mzout))
			tol <- c(absolute = resolution / 2)
		}
		mz.bins <- c(mzout[1] - error[1], mzout + error)
		if ( attach.only ) {
			data <- list(keys=mz, values=intensity)
			mz <- mzout
			spectra <- sparse_mat(data, keys=mz,
				nrow=length(mz), ncol=length(intensity),
				tolerance=tol, combiner="sum")
		} else {
			if ( outclass == "MSImageSet" ) {
				data <- list(keys=list(), values=list())
				for ( i in seq_along(mz) ) {
					mzi <- mz[[i]]
					wh <- findInterval(mzi, mz.bins)
					s <- as.vector(tapply(intensity[[i]], wh, sum))
					data$keys[[i]] <- mzout[unique(wh)]
					data$values[[i]] <- s
				}
			} else if ( outclass == "MSImagingExperiment") {
				data <- list(keys=mz[], values=intensity[])
			}
			mz <- mzout
			spectra <- sparse_mat(data, keys=mz,
				nrow=length(mz), ncol=length(intensity),
				tolerance=tol, combiner="sum")
		}
	}
	# set up coordinates
	x <- scans(info)[["position x"]]
	y <- scans(info)[["position y"]]
	z <- scans(info)[["position z"]]
	x3d <- scans(info)[["3DPositionX"]]
	y3d <- scans(info)[["3DPositionY"]]
	z3d <- scans(info)[["3DPositionZ"]]
	if ( allmissing(z) && allmissing(z3d) ) {
		coord <- data.frame(x=x, y=y)
	} else if ( allmissing(z3d) ) {
		coord <- data.frame(x=x, y=y, z=z)
	} else if ( nomissing(z3d) ) {
		z <- as.integer(xtfrm(round(z3d)))
		coord <- data.frame(x=x, y=y, z=z)
	} else {
		.stop("invalid pixel coordinates")
	}
	if ( !allmissing(x3d) && !allmissing(y3d) ) {
		if ( allmissing(z3d) ) {
			coordExact <- scans(info)[c("3DPositionX", "3DPositionY")]
		} else {
			coordExact <- scans(info)[c("3DPositionX", "3DPositionY", "3DPositionZ")]
		}
	} else {
		coordExact <- NULL
	}
	if ( outclass == "MSImageSet" ) {
		experimentData <- new("MIAPE-Imaging")
		processingData <- new("MSImageProcess", files=metadata(info)[["files"]])
		object <- MSImageSet(spectra=spectra, mz=mz, coord=coord,
			processingData=processingData,
			experimentData=experimentData)
		sampleNames(object) <- metadata(info)[["name"]]
	} else if ( outclass == "MSImagingExperiment" ) {
		object <- MSImagingExperiment(spectra,
			featureData=MassDataFrame(mz=mz),
			pixelData=PositionDataFrame(coord=coord,
				run=metadata(info)[["name"]]),
			metadata=metadata(info),
			centroided=isCentroided(info))
	} else {
		.stop("unrecognized outclass")
	}
	if ( !is.null(coordExact) )
		pixelData(object)[names(coordExact)] <- as.list(coordExact)
	object
}

.detectMassRangeAndResolution <- function(mz, units, BPPARAM)
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
	mzinfo <- chunk_apply(mz, fun,
		chunks=getCardinalNumBlocks(),
		verbose=getCardinalVerbose(),
		simplify=TRUE, BPPARAM=BPPARAM)
	mzinfo <- as.matrix(mzinfo)
	mzinfo[!is.finite(mzinfo)] <- NA
	res <- median(mzinfo[1,], na.rm=TRUE)
	min <- round(min(mzinfo[2,], na.rm=TRUE), digits=4)
	max <- round(max(mzinfo[3,], na.rm=TRUE), digits=4)
	if ( units == "ppm" ) {
		res <- roundnear(res, precision=1)
	} else {
		res <- round(res, digits=4)
	}
	list(mass.range=c(min, max), resolution=res)
}

