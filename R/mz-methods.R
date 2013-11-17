
#### implement mz methods ####

# get m/z-values
setMethod("mz", "MSImageSet", function(object) {
	object@featureData[object@metaData[["mzDimName"]]][[1]]
} )

# set m/z values
setReplaceMethod("mz", "MSImageSet", function(object, value) {
	object@featureData[object@metaData[["mzDimName"]]][[1]] <- value
	object@metaData[["history"]][[date()]] <- generateHistory(match.call(call=sys.call(-1)))
	validObject(object)
	return(object)
} )
