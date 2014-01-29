
#### read imzML files ####

readImzML <- function(name, folder=".") {
	# check for files
	xmlpath <- file.path(folder, paste(name, ".imzML", sep=""))
	if ( !file.exists(xmlpath) ) stop(hdrpath, " does not exist")
	ibdpath <- file.path(folder, paste(name, ".ibd", sep=""))
	if ( !file.exists(ibdpath) ) stop(t2mpath, " does not exist")
	# parse imzML
	mzml <- .Call("parseImzML", xmlpath)
	s1 <- mzml$run$spectrumList[[1]]
	# read m/z values
	mz <- .Call("readIbdMzArray", ibdpath,
		mzml$fileDescription$fileContent[["ibd binary type"]],
		s1[["binaryDataArrayList"]][["m/z array"]][["binary data type"]],
		s1[["binaryDataArrayList"]][["m/z array"]][["external offset"]],
		s1[["binaryDataArrayList"]][["m/z array"]][["external array length"]])
	# read intensity values
	data <- .Call("readIbdIntensityArray", ibdpath,
		mzml$fileDescription$fileContent[["ibd binary type"]],
		s1[["binaryDataArrayList"]][["intensity array"]][["binary data type"]],
		s1[["binaryDataArrayList"]][["intensity array"]][["external offset"]],
		s1[["binaryDataArrayList"]][["intensity array"]][["external array length"]],
		length(mzml$run$spectrumList))
	# set up coordinates
	x <- sapply(mzml$run$spectrumList, function(s) s$scanList$scan[["position x"]])
	y <- sapply(mzml$run$spectrumList, function(s) s$scanList$scan[["position y"]])
	z <- sapply(mzml$run$spectrumList, function(s) s$scanList$scan[["position z"]])
	if ( all(z == 0) ) { # find_position_z returns 0 if xml node doesn't exist
		coord <- data.frame(x=x, y=y)
	} else {
		coord <- data.frame(x=x, y=y, z=z)
	}
	# create and return dataset
	MSImageSet(spectra=data, mz=mz, coord=coord)
}
