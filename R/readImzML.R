
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
	# parse imzML
	.log("readImzML: Parsing file '", xmlpath, "'")
	mzml <- .Call("parseImzML", xmlpath)
	ibdtype <- mzml$fileDescription$fileContent[["ibd binary type"]]
	count <- length(mzml$run$spectrumList)
	s1 <- mzml$run$spectrumList[[1]]
	# read m/z values
	.log("readImzML: Reading m/z information from file '", ibdpath, "'")
	mz.datatype <- s1[["binaryDataArrayList"]][["m/z array"]][["binary data type"]]
	mz.offset <- sapply(mzml$run$spectrumList, function(s)
		s[["binaryDataArrayList"]][["m/z array"]][["external offset"]])
	mz.length <- sapply(mzml$run$spectrumList, function(s)
		s[["binaryDataArrayList"]][["m/z array"]][["external array length"]])
	mz <- .Call("readIbdMzArray", ibdpath, ibdtype,
		mz.datatype, mz.offset, mz.length, count)
	# read intensity values
	.log("readImzML: Reading intensity information from file '", ibdpath, "'")
	intensity.datatype <- s1[["binaryDataArrayList"]][["intensity array"]][["binary data type"]]
	intensity.offset <- sapply(mzml$run$spectrumList, function(s)
		s[["binaryDataArrayList"]][["intensity array"]][["external offset"]])
	intensity.length <- sapply(mzml$run$spectrumList, function(s)
		s[["binaryDataArrayList"]][["intensity array"]][["external array length"]])
	if ( attach.only ) {
		if ( ibdtype == "processed" )
			.stop("readImzML: Data binary type 'processed' is not currently supported for attach only.")
		intensity <- Binmat(files=ibdpath, datatype=intensity.datatype,
			offsets=intensity.offset, extents=intensity.length)
	} else {
		intensity <- .Call("readIbdIntensityArray", ibdpath, ibdtype,
			intensity.datatype, intensity.offset, intensity.length, count)
		if ( ibdtype == "processed" ) {
			mz.min <- min(sapply(mz, min))
			mz.max <- max(sapply(mz, max))
			if ( match.arg(units.accuracy) == "ppm" ) {
				if ( floor(mz.min) <= 0 )
					.stop("readImzML: m/z values must be positive for units.accuracy='ppm'")
				mzout <- seq.ppm(
					from=floor(mz.min),
					to=ceiling(mz.max),
					ppm=mass.accuracy * 2)
				error <- mass.accuracy * 1e-6 * mzout
			} else {
				mzout <- seq(
					from=floor(mz.min),
					to=ceiling(mz.max),
					by=mass.accuracy * 2)
				error <- rep(mass.accuracy, length(mzout))
			}
			mz.bins <- c(mzout[1] - error[1], mzout + error)
			mz.names <- .format.mz(mzout)
			intensity <- mapply(function(s, mzi) {
				which <- findInterval(mzi, mz.bins)
				s <- as.vector(tapply(s, which, sum))
				names(s) <- mz.names[unique(which)]
				s
			}, intensity, mz, SIMPLIFY=FALSE)
			mz <- mzout
			intensity <- new("Hashmat", data=intensity, keys=mz.names,
				dim=c(length(mz.names), length(intensity)))
		}
	}
	# set up coordinates
	x <- sapply(mzml$run$spectrumList, function(s) s$scanList$scan[["position x"]])
	y <- sapply(mzml$run$spectrumList, function(s) s$scanList$scan[["position y"]])
	z <- sapply(mzml$run$spectrumList, function(s) s$scanList$scan[["position z"]])
	x3d <- sapply(mzml$run$spectrumList, function(s) s$scanList$scan[["3DPositionX"]])
	y3d <- sapply(mzml$run$spectrumList, function(s) s$scanList$scan[["3DPositionY"]])
	z3d <- sapply(mzml$run$spectrumList, function(s) s$scanList$scan[["3DPositionZ"]])
	nz <- length(unique(z))
	nz3d <- length(unique(z3d))
	if ( nz == 1 && nz3d == 1 ) {
		coord <- data.frame(x=x, y=y)
	} else if ( nz3d == 1 ) {
		coord <- data.frame(x=x, y=y, z=z)
	} else {
		z <- as.integer(as.factor(z3d))
		coord <- data.frame(x=x, y=y, z=z)
	}
	# create and return dataset
	experimentData <- new("MIAPE-Imaging")
	processingData <- new("MSImageProcess", files=c(xmlpath, ibdpath))
	object <- MSImageSet(spectra=intensity, mz=mz, coord=coord,
		processingData=processingData,
		experimentData=experimentData)
	sampleNames(object) <- name
	object
}
