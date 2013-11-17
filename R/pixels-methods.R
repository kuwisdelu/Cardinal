
#### implement pixels methods ####

# access the position of pixels in the spectra slot by their coordinates
setMethod("pixels", c("MSImageSet", "numeric"), function(object, coord, ...) {
	if ( !is.null(names(coord)) ) coord <- coord[object@metaData[["coordDimNames"]]]
	f <- function(...) object@metaData[["positionArray"]][...]
	return(do.call(f, c(as.list(coord), ...)))
} )

# access the position of pixels in the spectra slot by their coordinates
setMethod("pixels", c("MSImageSet", "data.frame"), function(object, coord, ...) {
	if ( !is.null(names(coord)) ) coord <- coord[object@metaData[["coordDimNames"]]]
	return(apply(coord, 1, function(xyz) pixels(object, xyz, ...)))
} )

# access the position of pixels in the spectra slot by their coordinates
setMethod("pixels", c("MSImageSet", "list"), function(object, coord, ...) {
	if ( !is.null(names(coord)) ) {
		allcoord <- lapply(coord(object), function(xyz) 1:max(xyz))
		names(allcoord) <- object@metaData[["coordDimNames"]]
		allcoord[names(coord)] <- coord
		coord <- allcoord
	}
	f <- function(...) object@metaData[["positionArray"]][...]
	return(do.call(f, c(coord, ...)))
} )

