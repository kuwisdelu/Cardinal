
#### functions for reading imzML ####

readImzML <- function(file, imzMLConverter=NULL) {
	# check if a converter is available
	if ( is.null(imzMLConverter) ) {
		dir <- find.package("Cardinal")
		jar <- paste(dir, "imzMLConverter.jar", sep="/")
		if ( file.exists(jar) ) {
			imzMLConverter <- jar
		} else {
			stop("'imzMLConverter' not specified\nDownload imzMLConverter at http://www.imzmlconverter.co.uk")
		}
	}
	if ( !require(rJava) ) {
		stop("package 'rJava' is required for this function - please install it and try again.")
	}
	.jinit()
	.jaddClassPath(path=imzMLConverter)
	imzML <- J("imzMLConverter.ImzMLHandler")$parseimzML(file)
	# get the coordinates and m/z data
	xdim <- J(imzML, "getWidth")
	ydim <- J(imzML, "getHeight")
	coord <- expand.grid(x=1:xdim, y=1:ydim)
	mz <- J(J(imzML, "getSpectrum", as.integer(1), as.integer(1)), "getmzArray")
	# get the mass spectra
	spectra <- apply(coord, 1, function(xy) {
		J(J(imzML, "getSpectrum", as.integer(xy[[1]]), as.integer(xy[[2]])),
			"getIntensityArray")
	} )
	# create the MSImageSet
	filename <- strsplit(file, split="/|.imzML")[[1]]
	object <- createMSImageSet(spectra=spectra, mz=mz, coord=coord)
	object@metaData[["name"]] <- filename[[length(filename)]]
	object@metaData[["file"]] <- file
	object
}
