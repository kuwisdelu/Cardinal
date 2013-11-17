
#### implement regeneratePositions methods ####

setMethod("regeneratePositions", "MSImageSet", function(object) {
	object@metaData[["positionArray"]] <- generatePositionArray(object)
	return(object)
} )

