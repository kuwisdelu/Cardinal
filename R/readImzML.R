
#### Read imzML files ####
## ----------------------

readImzML <- function(name, folder=getwd(), attach.only=FALSE,
	mass.accuracy=200, units.accuracy=c("ppm", "mz"), ...)
{
	# check for files
	xmlpath <- normalizePath(file.path(folder, paste(name, ".imzML", sep="")),
		mustWork=FALSE)
	if ( !file.exists(xmlpath) ) .stop("readImzML: ", xmlpath, " does not exist")
	ibdpath <- normalizePath(file.path(folder, paste(name, ".ibd", sep="")),
		mustWork=FALSE)
	if ( !file.exists(ibdpath) ) .stop("readImzML: ", ibdpath, " does not exist")
	# read imzML file
	.log("readImzML: Reading imzML file '", xmlpath, "'")
	mzml <- .readImzML(xmlpath)
	# read ibd file
	attr(mzml, "name") <- name
	attr(mzml, "folder") <- folder
	attr(mzml, "files") <- c(xmlpath, ibdpath)
	units.accuracy <- match.arg(units.accuracy)
	.log("readImzML: Reading ibd file '", ibdpath, "'")
	obj <- .readIbd.MSImageset(ibdpath, mzml, attach.only=attach.only,
		mass.accuracy=mass.accuracy, units.accuracy=units.accuracy)
	if ( validObject(obj) )
		obj
}

.readImzML <- function(file) {
	info <- .Call("readImzML", normalizePath(file))
	dfnames <- c("spectrumList", "scanList",
		"mzArrayList", "intensityArrayList")
	todf <- which(names(info) %in% dfnames)
	info[todf] <- lapply(info[todf], as.data.frame,
		check.names=FALSE, stringsAsFactors=FALSE)
	len <- sapply(info$experimentMetadata, nchar, type="bytes")
	info$experimentMetadata <- info$experimentMetadata[len > 0]
	info
}

.readIbd.MSImageset <- function(file, info,
	attach.only, mass.accuracy, units.accuracy)
{
	ibdtype <- info$experimentMetadata[["ibd binary type"]]
	mz.ibdtype <- info$mzArrayList[["binary data type"]]
	intensity.ibdtype <- info$intensityArrayList[["binary data type"]]
	# read binary data
	if ( ibdtype == "continuous" ) {
		mz <- matter_vec(paths=file,
			datamode=modeof.ibdtype(mz.ibdtype[1]),
			offset=info$mzArrayList[["external offset"]][1],
			extent=info$mzArrayList[["external array length"]][1])
		intensity <- matter_mat(paths=file,
			datamode=modeof.ibdtype(intensity.ibdtype[1]),
			offset=info$intensityArrayList[["external offset"]],
			extent=info$intensityArrayList[["external array length"]])
		if ( attach.only ) {
			spectra <- intensity
		} else {
			spectra <- intensity[]
		}
		mz <- mz[]
	} else if ( ibdtype == "processed" ) {
		mz <- matter_list(paths=file,
			datamode=modeof.ibdtype(mz.ibdtype),
			offset=info$mzArrayList[["external offset"]],
			extent=info$mzArrayList[["external array length"]])
		intensity <- matter_list(paths=file,
			datamode=modeof.ibdtype(intensity.ibdtype),
			offset=info$intensityArrayList[["external offset"]],
			extent=info$intensityArrayList[["external array length"]])
		mz.range <- range(as(mz, "matter_vec"))
		mz.min <- mz.range[1]
		mz.max <- mz.range[2]
		if ( units.accuracy == "ppm" ) {
			if ( floor(mz.min) <= 0 )
				.stop("readImzML: m/z values must be positive for units.accuracy='ppm'")
			mzout <- seq.ppm(
				from=floor(mz.min),
				to=ceiling(mz.max),
				ppm=mass.accuracy) # ppm == half-bin-widths
			error <- mass.accuracy * 1e-6 * mzout
			tol <- c(relative = mass.accuracy * 1e-6)
		} else {
			mzout <- seq(
				from=floor(mz.min),
				to=ceiling(mz.max),
				by=mass.accuracy * 2)  # by == full-bin-widths
			error <- rep(mass.accuracy, length(mzout))
			tol <- c(absolute = mass.accuracy)
		}
		mz.bins <- c(mzout[1] - error[1], mzout + error)
		if ( attach.only ) {
			data <- list(keys=mz, values=intensity)
			mz <- mzout
			spectra <- sparse_mat(data, keys=mz,
				nrow=length(mz), ncol=length(intensity),
				tolerance=tol, combiner="sum")
		} else {
			data <- list(keys=list(), values=list())
			for ( i in seq_along(mz) ) {
				mzi <- mz[[i]]
				wh <- findInterval(mzi, mz.bins)
				s <- as.vector(tapply(intensity[[i]], wh, sum))
				data$keys[[i]] <- mzout[unique(wh)]
				data$values[[i]] <- s
			}
			mz <- mzout
			spectra <- sparse_mat(data, keys=mz,
				nrow=length(mz), ncol=length(intensity),
				tolerance=tol, combiner="sum")
		}
	}
	# set up coordinates
	x <- info$scanList[["position x"]]
	y <- info$scanList[["position y"]]
	z <- info$scanList[["position z"]]
	x3d <- info$scanList[["3DPositionX"]]
	y3d <- info$scanList[["3DPositionY"]]
	z3d <- info$scanList[["3DPositionZ"]]
	if ( all(is.na(z)) && all(is.na(z3d)) ) {
		coord <- data.frame(x=x, y=y)
	} else if ( all(is.na(z3d)) ) {
		coord <- data.frame(x=x, y=y, z=z)
	} else {
		z <- as.integer(as.factor(z3d))
		coord <- data.frame(x=x, y=y, z=z)
	}
	experimentData <- new("MIAPE-Imaging")
	processingData <- new("MSImageProcess", files=attr(info, "files"))
	object <- MSImageSet(spectra=spectra, mz=mz, coord=coord,
		processingData=processingData,
		experimentData=experimentData)
	sampleNames(object) <- attr(info, "name")
	object
}

