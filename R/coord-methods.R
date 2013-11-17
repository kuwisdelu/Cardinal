
#### implement coord ####

# get the pixel coordinates
setMethod("coord", "MSImageSet", function(object) {
	object@pixelData[object@metaData[["coordDimNames"]]]
} )

# set the pixel coordinates
setReplaceMethod("coord", "MSImageSet", function(object, value) {
	object@pixelData[object@metaData[["coordDimNames"]]] <- value
	object@metaData[["history"]][[date()]] <- generateHistory(match.call(call=sys.call(-1)))
	object@metaData[["positionArray"]] <- generatePositionArray(object@pixelData[
		object@metaData[["coordDimNames"]]])
	validObject(object)
	return(object)
} )

