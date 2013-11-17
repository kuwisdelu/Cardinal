
#### implement checks for whether a dataset is peak-picked ####

setMethod("isPeaks", "MSImageSet", function(object) isTRUE(object@metaData[["isPeaks"]]))

setReplaceMethod("isPeaks", "MSImageSet", function(object, value) {
	object@metaData[["isPeaks"]] <- value
	validObject(object)
	return(object)
} )

setMethod("isResampled", "MSImageSet", function(object) isTRUE(object@metaData[["isResampled"]]))

setReplaceMethod("isResampled", "MSImageSet", function(object, value) {
	object@metaData[["isResampled"]] <- value
	validObject(object)
	return(object)
} )

setMethod("isBinned", "MSImageSet", function(object) isTRUE(object@metaData[["isBinned"]]))

setReplaceMethod("isBinned", "MSImageSet", function(object, value) {
	object@metaData[["isBinned"]] <- value
	validObject(object)
	return(object)
} )

