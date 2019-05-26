
#### Read imzML files ####
## ----------------------

readImzML <- function(name, folder = getwd(), attach.only = TRUE,
	mass.range = NULL, resolution = 200, units = c("ppm", "mz"),
	as = c("MSImagingExperiment", "MSImageSet"), parse.only=FALSE,
	BPPARAM = bpparam(), ...)
{
	# check input
	dots <- list(...)
	if ( "mass.accuracy" %in% names(dots) ) {
		.stop("'mass.accuracy' is defunct.\n",
			"Use 'resolution' instead.")
	}
	if ( "units.accuracy" %in% names(dots) ) {
		.stop("'units.accuracy' is defunct.\n",
			"Use 'units' instead.")
	}
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
		.message("detected 'continuous' imzML")
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
		.message("detected 'processed' imzML")
		mz <- matter_list(paths=file,
			datamode=Ctypeof(mz.ibdtype),
			offset=mzData(info)[["external offset"]],
			extent=mzData(info)[["external array length"]])
		intensity <- matter_list(paths=file,
			datamode=Ctypeof(intensity.ibdtype),
			offset=intensityData(info)[["external offset"]],
			extent=intensityData(info)[["external array length"]])
		if ( is.null(mass.range) ) {
			.message("determining mass range...")
			if ( attach.only ) {
				mz.ranges <- sapply(mz, range, BPPARAM=BPPARAM)				
			} else {
				mz.ranges <- sapply(mz[], range)
			}
			mz.range <- range(mz.ranges[is.finite(mz.ranges)])
			.message("detected mass range: ",
				round(mz.range[1L], 4), " to ", round(mz.range[2L], 4))
		} else {
			mz.range <- mass.range
		}
		mz.min <- mz.range[1]
		mz.max <- mz.range[2]
		if ( units == "ppm" ) {
			if ( floor(mz.min) <= 0 )
				.stop("m/z values must be positive for units='ppm'")
			mzout <- seq.ppm(
				from=floor(mz.min),
				to=ceiling(mz.max),
				ppm=resolution) # ppm == half-bin-widths
			error <- resolution * 1e-6 * mzout
			tol <- c(relative = resolution * 1e-6)
		} else {
			mzout <- seq(
				from=floor(mz.min),
				to=ceiling(mz.max),
				by=resolution)  # by == full-bin-widths
			error <- rep(resolution, length(mzout))
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

