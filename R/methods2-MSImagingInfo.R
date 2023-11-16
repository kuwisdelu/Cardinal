#### Methods for MSImagingInfo ####
## ----------------------------------

.valid.MSImagingInfo <- function(object) {
	errors <- NULL
	nrows <- nrow(object@scanList)
	if ( nrow(object@mzArrayList) != nrows )
		errors <- c(errors , paste("number of rows of 'scanList'",
			"and 'mzArrayList' must be equal"))
	if ( nrow(object@intensityArrayList) != nrows )
		errors <- c(errors , paste("number of rows of 'scanList'",
			"and 'intensityArrayList' must be equal"))
	if ( is.null(errors) ) TRUE else errors
}

setValidity("MSImagingInfo", .valid.MSImagingInfo)

# create MSImagingInfo

setMethod("msiInfo", "MSImagingExperiment",
	function(object, mz.type = "32-bit float",
					intensity.type = "32-bit float", ...)
	{
		info <- .new.MSContinuousImagingInfo(object, mz.type, intensity.type)
		info@metadata[["ibd binary type"]] <- "continuous"
		info@metadata <- append(info@metadata, metadata(object))
		info@metadata <- info@metadata[unique(names(info@metadata))]
		if ( validObject(info) )
			info
	})

setMethod("msiInfo", "MSContinuousImagingExperiment",
	function(object, mz.type = "32-bit float",
					intensity.type = "32-bit float", new = TRUE, ...)
	{
		if ( new ) {
			info <- .new.MSContinuousImagingInfo(object, mz.type, intensity.type)
		} else {
			info <- .get.MSContinuousImagingInfo(object)
		}
		info@metadata[["ibd binary type"]] <- "continuous"
		info@metadata <- append(info@metadata, metadata(object))
		info@metadata <- info@metadata[unique(names(info@metadata))]
		if ( validObject(info) )
			info
	})

setMethod("msiInfo", "MSProcessedImagingExperiment",
	function(object, mz.type = "32-bit float",
					intensity.type = "32-bit float", new = TRUE, ...)
	{
		if ( new ) {
			info <- .new.MSProcessedImagingInfo(object, mz.type, intensity.type)
		} else {
			info <- .get.MSProcessedImagingInfo(object)
		}
		info@metadata[["ibd binary type"]] <- "processed"
		info@metadata <- append(info@metadata, metadata(object))
		info@metadata <- info@metadata[unique(names(info@metadata))]
		if ( validObject(info) )
			info
	})

.make.scanList <- function(x) {
	# extract coordinates
	positionNames <- c("position x", "position y", "position z")
	if ( is3D(x) ) {
		scanList <- DataFrame(coord(x)[1:3])
	} else {
		scanList <- DataFrame(coord(x)[c(1,2)])
	}
	names(scanList) <- positionNames[seq_along(scanList)]
	# check for non-gridded x/y
	xNoGrid <- !all(is.wholenumber(scanList[["position x"]]))
	yNoGrid <- !all(is.wholenumber(scanList[["position y"]]))
	zNoGrid <- !all(is.wholenumber(scanList[["position z"]]))
	if ( xNoGrid || yNoGrid || zNoGrid ) {
		scanList[["3DPositionX"]] <- as.numeric(scanList[["position x"]])
		scanList[["3DPositionY"]] <- as.numeric(scanList[["position y"]])
		if ( is3D(x) )
			scanList[["3DPositionZ"]] <- as.numeric(scanList[["position z"]])
	}
	scanList[["position x"]] <- as.integer(round(scanList[["position x"]]))
	scanList[["position y"]] <- as.integer(round(scanList[["position y"]]))
	if ( is3D(x) )
		scanList[["position z"]] <- as.integer(round(scanList[["position z"]]))
	scanList
}

.new.MSContinuousImagingInfo <- function(x, mz.type, intensity.type)
{
	mz.type <- match.arg(mz.type,
		choices=c("32-bit float", "64-bit float"))
	intensity.type <- match.arg(intensity.type,
		choices=c("32-bit float", "64-bit float",
			"16-bit integer", "32-bit integer", "64-bit integer"))
	scanList <- .make.scanList(x)
	mzArrayList <- DataFrame(
		"external offset"=unname(rep(16, ncol(x))),
		"external array length"=unname(rep(nrow(x), ncol(x))),
		"external encoded length"=unname(rep(Csizeof(mz.type) * nrow(x), ncol(x))),
		"binary data type"=rep(mz.type, ncol(x)),
		check.names=FALSE)
	intensityArrayList <- DataFrame(
		"external offset"=unname(rep(16 + Csizeof(mz.type) * nrow(x), ncol(x))),
		"external array length"=unname(rep(nrow(x), ncol(x))),
		"external encoded length"=unname(rep(Csizeof(intensity.type) * nrow(x), ncol(x))),
		"binary data type"=rep(intensity.type, ncol(x)),
		check.names=FALSE)
	offset <- c(0, cumsum(as.numeric(intensityArrayList[["external encoded length"]][-ncol(x)])))
	intensityArrayList[["external offset"]] <- offset + intensityArrayList[["external offset"]]
	spectrumRepresentation <- ifelse(centroided(x),
		"centroid spectrum", "profile spectrum")
	experimentMetadata <- list("spectrum representation"=spectrumRepresentation)
	.MSImagingInfo(
		scanList=scanList,
		mzArrayList=mzArrayList,
		intensityArrayList=intensityArrayList,
		metadata=experimentMetadata)
}

.get.MSContinuousImagingInfo <- function(x)
{
	if ( !is(iData(x), "matter_mat") )
		.stop("intensity data are not a matter_mat object")
	if ( length(file) > 1 )
		.stop("intensity data are from more than one file")
	ibd <- as.list(atomdata(iData(x)))
	mz.width <- (ibd$offset[1] - 16L) / nrow(x)
	if ( mz.width == 4 ) {
		mz.type <- "32-bit float"
	} else if ( mz.width == 8 ) {
		mz.type <- "64-bit float"
	} else {
		.stop("undefined m/z array byte width: ", mz.width)
	}
	pmz <- matter_vec(offset=16, extent=nrow(x),
		type=Ctypeof(mz.type), path=path(iData(x)))
	mzcheck <- all.equal(pmz[], mz(x), tolerance=1e-3,
						check.attributes=FALSE)
	if ( isTRUE(mzcheck) ) {
		mzArrayList <- DataFrame(
			"external offset"=unname(rep(16, ncol(x))),
			"external array length"=unname(rep(nrow(x), ncol(x))),
			"external encoded length"=unname(rep(Csizeof(mz.type) * nrow(x), ncol(x))),
			"binary data type"=rep(mz.type, ncol(x)),
			check.names=FALSE)
	} else {
		.stop("m/z array in binary file do not match mz() of object")
	}
	intensity.mode <- as.character(unique(ibd$datamode))
	if ( length(intensity.mode) != 1 )
		.stop("multiple binary types found for intensity array")
	intensity.type <- Nametypeof(intensity.mode)
	ibd$extent <- as.integer(ibd$extent)
	intensityArrayList <- DataFrame(
		"external offset"=ibd$offset,
		"external array length"=ibd$extent,
		"external encoded length"=unname(Csizeof(intensity.type) * ibd$extent),
		"binary data type"=rep(intensity.type, ncol(x)),
		check.names=FALSE)
	scanList <- .make.scanList(x)
	spectrumRepresentation <- ifelse(centroided(x),
		"centroid spectrum", "profile spectrum")
	experimentMetadata <- list("spectrum representation"=spectrumRepresentation)
	id <- matter_vec(length=16, path=path(iData(x)), type="raw")
	hash <- checksum(iData(x), algo="sha1")
	experimentMetadata[["universally unique identifier"]] <- make.uuid(id[])
	experimentMetadata[["ibd SHA-1"]] <- tolower(as.character(hash))
	.MSImagingInfo(
		scanList=scanList,
		mzArrayList=mzArrayList,
		intensityArrayList=intensityArrayList,
		metadata=experimentMetadata)
}

.new.MSProcessedImagingInfo <- function(x, mz.type, intensity.type)
{
	mz.type <- match.arg(mz.type,
		choices=c("32-bit float", "64-bit float"))
	intensity.type <- match.arg(intensity.type,
		choices=c("32-bit float", "64-bit float",
			"16-bit integer", "32-bit integer", "64-bit integer"))
	scanList <- .make.scanList(x)
	if ( any(lengths(mzData(x)) != lengths(intensityData(x))) )
		.stop("lengths of intensity and m/z arrays differ")
	mzLengths <- intensityLengths <- lengths(iData(x))
	mzExtent <- Csizeof(mz.type) * mzLengths
	intensityExtent <- Csizeof(intensity.type) * intensityLengths
	mzOffset <- 16 + cumsum(c(0, mzExtent[-ncol(x)] + intensityExtent[-ncol(x)]))
	intensityOffset <- 16 + cumsum(mzExtent + c(0, intensityExtent[-ncol(x)]))
	mzArrayList <- DataFrame(
		"external offset"=unname(mzOffset),
		"external array length"=unname(mzLengths),
		"external encoded length"=unname(mzExtent),
		"binary data type"=rep(mz.type, ncol(x)),
		check.names=FALSE)
	intensityArrayList <- DataFrame(
		"external offset"=unname(intensityOffset),
		"external array length"=unname(intensityLengths),
		"external encoded length"=unname(intensityExtent),
		"binary data type"=rep(intensity.type, ncol(x)),
		check.names=FALSE)
	spectrumRepresentation <- ifelse(centroided(x),
		"centroid spectrum", "profile spectrum")
	experimentMetadata <- list("spectrum representation"=spectrumRepresentation)
	.MSImagingInfo(
		scanList=scanList,
		mzArrayList=mzArrayList,
		intensityArrayList=intensityArrayList,
		metadata=experimentMetadata)
}

.get.MSProcessedImagingInfo <- function(x)
{
	if ( !is(iData(x), "sparse_mat") )
		.stop("intensity data are not a sparse_mat object")
	if ( !is(mzData(x), "matter_list") )
		.stop("m/z array is not a matter_list object")
	if ( !is(intensityData(x), "matter_list") )
		.stop("intensity array is not a matter_list object")
	if ( path(mzData(x)) > 1 || path(intensityData(x)) > 1 )
		.stop("m/z data or intensity data are from more than one file")
	if ( path(mzData(x)) != path(intensityData(x)) )
		.stop("m/z data and intensity data are from different files")
	mz.ibd <- as.list(atomdata(mzData(x)))
	mz.mode <- as.character(unique(mz.ibd$datamode))
	if ( length(mz.mode) != 1 )
		.stop("multiple binary types found for intensity array")
	mz.type <- Nametypeof(mz.mode)
	mz.ibd$extent <- as.integer(mz.ibd$extent)
	mzArrayList <- DataFrame(
		"external offset"=mz.ibd$offset,
		"external array length"=mz.ibd$extent,
		"external encoded length"=unname(Csizeof(mz.type) * mz.ibd$extent),
		"binary data type"=rep(mz.type, ncol(x)),
		check.names=FALSE)
	intensity.ibd <- as.list(atomdata(intensityData(x)))
	intensity.mode <- as.character(unique(intensity.ibd$datamode))
	if ( length(intensity.mode) != 1 )
		.stop("multiple binary types found for intensity array")
	intensity.type <- Nametypeof(intensity.mode)
	intensity.ibd$extent <- as.integer(intensity.ibd$extent)
	intensityArrayList <- DataFrame(
		"external offset"=intensity.ibd$offset,
		"external array length"=intensity.ibd$extent,
		"external encoded length"=unname(Csizeof(intensity.type) * intensity.ibd$extent),
		"binary data type"=rep(intensity.type, ncol(x)),
		check.names=FALSE)
	scanList <- .make.scanList(x)
	spectrumRepresentation <- ifelse(centroided(x),
		"centroid spectrum", "profile spectrum")
	experimentMetadata <- list("spectrum representation"=spectrumRepresentation)
	id <- matter_vec(length=16, path=path(intensityData(x)), type="raw")
	hash <- checksum(intensityData(x), algo="sha1")
	experimentMetadata[["universally unique identifier"]] <- make.uuid(id[])
	experimentMetadata[["ibd SHA-1"]] <- tolower(as.character(hash))
	.MSImagingInfo(
		scanList=scanList,
		mzArrayList=mzArrayList,
		intensityArrayList=intensityArrayList,
		metadata=experimentMetadata)
}

setMethod("length", "MSImagingInfo", function(x) nrow(x@scanList))

setMethod("as.list", "MSImagingInfo",
	function(x, ...)
	{
		list(scanList=as.list(x@scanList),
			mzArrayList=as.list(x@mzArrayList),
			intensityArrayList=as.list(x@intensityArrayList),
			experimentMetadata=x@metadata)
	})

# scans list

setMethod("scans", "MSImagingInfo",
	function(object) object@scanList)

# m/z array list

setMethod("mzData", "MSImagingInfo",
	function(object, ...) object@mzArrayList)

# intensity array list

setMethod("intensityData", "MSImagingInfo",
	function(object, ...) object@intensityArrayList)

# centroided

setMethod("isCentroided", "MSImagingInfo",
	function(object, ...) .isCentroided(object))

.isCentroided <- function(object) {
	if ( !hasMethod("spectrumRepresentation", class(object)) )
		return(NA)
	desc <- spectrumRepresentation(object)
	if ( is.null(desc) ) {
		NA
	} else if ( isTRUE(desc == "centroid spectrum") ) {
		TRUE
	} else if ( isTRUE(desc == "profile spectrum") ) {
		FALSE
	} else {
		NA
	}
}

# processing metadata

setMethod("normalization", "Vector",
	function(object) object@metadata[["intensity normalization"]])

setReplaceMethod("normalization", "Vector",
	function(object, value) {
		object@metadata[["intensity normalization"]] <- value
		object
	})

setMethod("smoothing", "Vector",
	function(object) object@metadata[["smoothing"]])

setReplaceMethod("smoothing", "Vector",
	function(object, value) {
		object@metadata[["smoothing"]] <- value
		object
	})

setMethod("baselineReduction", "Vector",
	function(object) object@metadata[["baseline reduction"]])

setReplaceMethod("baselineReduction", "Vector",
	function(object, value) {
		object@metadata[["baseline reduction"]] <- value
		object
	})

setMethod("peakPicking", "Vector",
	function(object) object@metadata[["peak picking"]])

setReplaceMethod("peakPicking", "Vector",
	function(object, value) {
		object@metadata[["peak picking"]] <- value
		object
	})

setMethod("spectrumRepresentation", "Vector",
	function(object) object@metadata[["spectrum representation"]])

setReplaceMethod("spectrumRepresentation", "Vector",
	function(object, value) {
		object@metadata[["spectrum representation"]] <- value
		object
	})

# experiment metadata

setMethod("instrumentModel", "Vector",
	function(object) object@metadata[["instrument model"]])

setMethod("instrumentVendor", "Vector",
	function(object) object@metadata[["instrument vendor"]])

setMethod("matrixApplication", "Vector",
	function(object) object@metadata[["matrix application type"]])

setMethod("massAnalyzerType", "Vector",
	function(object) object@metadata[["mass analyzer type"]])

setMethod("ionizationType", "Vector",
	function(object) object@metadata[["ionization type"]])

setMethod("scanPolarity", "Vector",
	function(object) object@metadata[["scan polarity"]])

setMethod("scanType", "Vector",
	function(object) object@metadata[["scan type"]])

setMethod("scanPattern", "Vector",
	function(object) object@metadata[["scan pattern"]])

setMethod("scanDirection", "Vector",
	function(object) object@metadata[["linescan sequence"]])

setMethod("lineScanDirection", "Vector",
	function(object) object@metadata[["line scan direction"]])

setMethod("pixelSize", "Vector",
	function(object) object@metadata[["pixel size"]])



