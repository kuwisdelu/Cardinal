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

setMethod("msiInfo", "MSImageSet",
	function(object, mz.type = "32-bit float", intensity.type = "32-bit float")
	{
		info <- .make.MSContinuousImagingInfo(object, mz.type, intensity.type)
		info@metadata[["ibd.binary.type"]] <- "continuous"
		if ( validObject(info) )
			info
	})

setMethod("msiInfo", "MSImagingExperiment",
	function(object, mz.type = "32-bit float", intensity.type = "32-bit float")
	{
		info <- .make.MSContinuousImagingInfo(object, mz.type, intensity.type)
		info@metadata[["ibd.binary.type"]] <- "continuous"
		info@metadata <- append(info@metadata, metadata(object))
		info@metadata <- info@metadata[unique(names(info@metadata))]
		if ( validObject(info) )
			info
	})

setMethod("msiInfo", "MSContinuousImagingExperiment",
	function(object, mz.type = "32-bit float", intensity.type = "32-bit float")
	{
		info <- .make.MSContinuousImagingInfo(object, mz.type, intensity.type)
		info@metadata[["ibd.binary.type"]] <- "continuous"
		info@metadata <- append(info@metadata, metadata(object))
		info@metadata <- info@metadata[unique(names(info@metadata))]
		if ( validObject(info) )
			info
	})

setMethod("msiInfo", "MSProcessedImagingExperiment",
	function(object, mz.type = "32-bit float", intensity.type = "32-bit float")
	{
		stop("not supported yet")
	})

.make.MSContinuousImagingInfo <- function(x, mz.type, intensity.type) {
	mz.type <- match.arg(mz.type,
		choices=c("32-bit float", "64-bit float"))
	intensity.type <- match.arg(intensity.type,
		choices=c("32-bit float", "64-bit float",
			"16-bit integer", "32-bit integer", "64-bit integer"))
	coordNames <- coordLabels(x)[c(1,2)]
	positionNames <- c("position x", "position y", "position z")
	scanList <- DataFrame(coord(x)[coordNames])
	names(scanList) <- positionNames[seq_along(coordNames)]
	mzArrayList <- DataFrame(
		"external offset"=rep(16, ncol(x)),
		"external array length"=rep(nrow(x), ncol(x)),
		"external encoded length"=rep(Csizeof(mz.type) * nrow(x), ncol(x)),
		"binary data type"=rep(mz.type, ncol(x)),
		check.names=FALSE)
	intensityArrayList <- DataFrame(
		"external offset"=rep(16 + Csizeof(mz.type) * nrow(x), ncol(x)),
		"external array length"=rep(nrow(x), ncol(x)),
		"external encoded length"=rep(Csizeof(intensity.type) * nrow(x), ncol(x)),
		"binary data type"=rep(intensity.type, ncol(x)),
		check.names=FALSE)
	offset <- c(0, cumsum(intensityArrayList[["external encoded length"]][-ncol(x)]))
	intensityArrayList[["external offset"]] <- offset + intensityArrayList[["external offset"]]
	spectrumRepresentation <- ifelse(centroided(x),
		"centroid spectrum", "profile spectrum")
	experimentMetadata <- list("spectrum representation"=spectrumRepresentation)
	new("MSImagingInfo",
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
	function(object) object@mzArrayList)

# intensity array list

setMethod("imageData", "MSImagingInfo",
	function(y) y@intensityArrayList)

# experiment metadata

setMethod("instrumentModel", "Vector",
	function(object) object@metadata[["instrument model"]])

setReplaceMethod("instrumentModel", "Vector",
	function(object, value) {
		object@metadata[["instrument model"]] <- value
		object
	})

setMethod("instrumentVendor", "Vector",
	function(object) object@metadata[["instrument vendor"]])

setReplaceMethod("instrumentVendor", "Vector",
	function(object, value) {
		object@metadata[["instrument vendor"]] <- value
		object
	})

setMethod("matrixApplication", "Vector",
	function(object) object@metadata[["matrix application type"]])

setReplaceMethod("matrixApplication", "Vector",
	function(object, value) {
		object@metadata[["matrix application type"]] <- value
		object
	})

setMethod("massAnalyzerType", "Vector",
	function(object) object@metadata[["mass analyzer type"]])

setReplaceMethod("massAnalyzerType", "Vector",
	function(object, value) {
		object@metadata[["mass analyzer type"]] <- value
		object
	})

setMethod("ionizationType", "Vector",
	function(object) object@metadata[["ionization type"]])

setReplaceMethod("ionizationType", "Vector",
	function(object, value) {
		object@metadata[["ionization type"]] <- value
		object
	})

setMethod("scanPolarity", "Vector",
	function(object) object@metadata[["scan polarity"]])

setReplaceMethod("scanPolarity", "Vector",
	function(object, value) {
		object@metadata[["scan polarity"]] <- value
		object
	})

setMethod("scanType", "Vector",
	function(object) object@metadata[["scan type"]])

setReplaceMethod("scanType", "Vector",
	function(object, value) {
		object@metadata[["scan type"]] <- value
		object
	})

setMethod("scanPattern", "Vector",
	function(object) object@metadata[["scan pattern"]])

setReplaceMethod("scanPattern", "Vector",
	function(object, value) {
		object@metadata[["scan pattern"]] <- value
		object
	})

setMethod("scanDirection", "Vector",
	function(object) object@metadata[["linescan sequence"]])

setReplaceMethod("scanDirection", "Vector",
	function(object, value) {
		object@metadata[["linescan sequence"]] <- value
		object
	})

setMethod("lineScanDirection", "Vector",
	function(object) object@metadata[["line scan direction"]])

setReplaceMethod("lineScanDirection", "Vector",
	function(object, value) {
		object@metadata[["line scan direction"]] <- value
		object
	})

setMethod("pixelSize", "Vector",
	function(object) object@metadata[["pixel size"]])

setReplaceMethod("pixelSize", "Vector",
	function(object, value) {
		object@metadata[["pixel size"]] <- value
		object
	})


