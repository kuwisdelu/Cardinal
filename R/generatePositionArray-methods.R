
#### implement generatePositionArray methods ####

setMethod("generatePositionArray", "data.frame", function(object, dim) {
	if ( missing(dim) ) dim <- sapply(object, max)
	positionArray <- array(1:prod(dim), dim=dim)
	f <- function(...) positionArray[...]
	fill.indices <- apply(object, 1, function(xyz) do.call(f, as.list(xyz)))
	positionArray <- array(NA, dim=dim)
	positionArray[fill.indices] <- 1:nrow(object)
	return(positionArray)
} )

setMethod("generatePositionArray", "matrix", function(object, dim) {
	if ( missing(dim) ) dim <- apply(object, 2, max)
	generatePositionArray(as.data.frame(object), dim)
} )

setMethod("generatePositionArray", "MSImageSet", function(object) {
	generatePositionArray(coord(object))
} )
