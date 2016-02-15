
#### Read imzML files ####
## ----------------------

readImzML <- function(name, folder=getwd()) {
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
	.log("readImzML: Reading m/z values from file '", ibdpath, "'")
	mz.datatype <- s1[["binaryDataArrayList"]][["m/z array"]][["binary data type"]]
	mz.offset <- sapply(mzml$run$spectrumList, function(s)
		s[["binaryDataArrayList"]][["m/z array"]][["external offset"]])
	mz.length <- sapply(mzml$run$spectrumList, function(s)
		s[["binaryDataArrayList"]][["m/z array"]][["external array length"]])
	mz <- .Call("readIbdMzArray", ibdpath, ibdtype,
		mz.datatype, mz.offset, mz.length, count)
	# read intensity values
	.log("readImzML: Reading intensity values from file '", ibdpath, "'")
	intensity.datatype <- s1[["binaryDataArrayList"]][["intensity array"]][["binary data type"]]
	intensity.offset <- sapply(mzml$run$spectrumList, function(s)
		s1[["binaryDataArrayList"]][["intensity array"]][["external offset"]])
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
			mz.names <- lapply(mz, as.character)
			mz.keys <- unique(unlist(mz.names))
			mz <- sort(unique(unlist(mz)))
			intensity <- mapply(function(dat, key) {
				names(dat) <- key
				dat
			}, intensity, mz.names, SIMPLIFY=FALSE)
			intensity <- new("Hashmat", data=intensity, keys=mz.keys,
				dim=c(length(mz.keys), length(intensity)))
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
