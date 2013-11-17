
#### Implement methods for spatial transformations of an MS image ####

setMethod("flipHorizontal", "MSImageSet", function(object, sliceDimNames=metaData(object)[[
	"coordDimNames"]][c(1,2)], ...)
{
	if ( length(sliceDimNames) != 2 | !is.character(sliceDimNames) ) {
		stop("'sliceDimNames' must be a character vector indicating the names of 2 dimensions to sliceDimNames")
	}
	x1 <- object@pixelData[,sliceDimNames[[1]]]
	x1 <- max(x1) - x1 + 1
	x2 <- object@pixelData[,sliceDimNames[[2]]]
	object@pixelData[,sliceDimNames] <- cbind(x1, x2)
	object@metaData[["history"]][[date()]] <- generateHistory(match.call(call=sys.call(-1)))
	object@metaData[["positionArray"]] <- generatePositionArray(object@pixelData[
		object@metaData[["coordDimNames"]]])
	validObject(object)
	return(object)
} )

setMethod("flipVertical", "MSImageSet", function(object, sliceDimNames=metaData(object)[[
	"coordDimNames"]][c(1,2)], ...)
{
	if ( length(sliceDimNames) != 2 | !is.character(sliceDimNames) ) {
		stop("'sliceDimNames' must be a character vector indicating the names of 2 dimensions to sliceDimNames")
	}
	x1 <- object@pixelData[,sliceDimNames[[1]]]
	x2 <- object@pixelData[,sliceDimNames[[2]]]
	x2 <- max(x2) - x2 + 1
	object@pixelData[,sliceDimNames] <- cbind(x1, x2)
	object@metaData[["history"]][[date()]] <- generateHistory(match.call(call=sys.call(-1)))
	object@metaData[["positionArray"]] <- generatePositionArray(object@pixelData[
		object@metaData[["coordDimNames"]]])
	validObject(object)
	return(object)
} )

setMethod("rotateLeft", "MSImageSet", function(object, sliceDimNames=metaData(object)[[
	"coordDimNames"]][c(1,2)], ...)
{
	if ( length(sliceDimNames) != 2 | !is.character(sliceDimNames) ) {
		stop("'sliceDimNames' must be a character vector indicating the names of 2 dimensions to sliceDimNames")
	}
	x1 <- object@pixelData[,sliceDimNames[[2]]]
	x1 <- max(x1) - x1 + 1
	x2 <- object@pixelData[,sliceDimNames[[1]]]
	object@pixelData[,sliceDimNames] <- cbind(x1, x2)
	object@metaData[["history"]][[date()]] <- generateHistory(match.call(call=sys.call(-1)))
	object@metaData[["positionArray"]] <- generatePositionArray(object@pixelData[
		object@metaData[["coordDimNames"]]])
	validObject(object)
	return(object)
} )

setMethod("rotateRight", "MSImageSet", function(object, sliceDimNames=metaData(object)[[
	"coordDimNames"]][c(1,2)], ...)
{
	if ( length(sliceDimNames) != 2 | !is.character(sliceDimNames) ) {
		stop("'sliceDimNames' must be a character vector indicating the names of 2 dimensions to sliceDimNames")
	}
	x1 <- object@pixelData[,sliceDimNames[[2]]]
	x2 <- object@pixelData[,sliceDimNames[[1]]]
	x2 <- max(x2) - x2 + 1
	object@pixelData[,sliceDimNames] <- cbind(x1, x2)
	object@metaData[["history"]][[date()]] <- generateHistory(match.call(call=sys.call(-1)))
	object@metaData[["positionArray"]] <- generatePositionArray(object@pixelData[
		object@metaData[["coordDimNames"]]])
	validObject(object)
	return(object)
} )

