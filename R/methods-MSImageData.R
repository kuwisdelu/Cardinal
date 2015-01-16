
setMethod("initialize", "MSImageData",
	function(.Object, ...) {
		callNextMethod(.Object, ...)
	})

MSImageData <- function(
	data = Hashmat(nrow=0, ncol=0),
	coord = expand.grid(
		x = seq_len(ncol(data)),
		y = seq_len(ifelse(ncol(data) > 0, 1, 0))),
	storageMode = "immutableEnvironment",
	positionArray = generatePositionArray(coord),
	dimnames = NULL,
	...)
{
	.MSImageData(iData=data,
		coord=coord,
		storageMode=storageMode,
		positionArray=positionArray,
		dimnames=dimnames,
		...)
}

setMethod("peakData", "MSImageData",
	function(object) object[["peakData"]])

setReplaceMethod("peakData", "MSImageData",
	function(object, value) {
		object[["peakData"]] <- value
		if ( validObject(object) )
			object			
	})

setMethod("mzData", "MSImageData",
	function(object) object[["mzData"]])

setReplaceMethod("mzData", "MSImageData",
	function(object, value) {
		object[["mzData"]] <- value
		if ( validObject(object) )
			object			
	})
