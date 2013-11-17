
#### implement methods for selecting pixels ####

setMethod("selectPixels", "MSImageSet", function(object,
	sliceDimNames=metaData(object)[["coordDimNames"]][c(1,2)],
	fixCoord=list(), silent=FALSE, ...)
{
	if ( length(sliceDimNames) != 2 | !is.character(sliceDimNames) ) {
		stop("'sliceDimNames' must list the name of 2 dimensions in 'coordDimNames'")	
	}
	if ( !silent ) message("Select pixels and press ESC or second mouse button when done")
	loc <- locator(type="p", pch=4, col="white")
	if( is.null(loc) ) stop("no pixels selected")
	return(selectPixelsHelper(loc, object, sliceDimNames, fixCoord))
} )

#### helper functions ####

selectPixelsHelper <- function(loc, object, sliceDimNames, fixCoord) {
	coord <- data.frame(round(loc$x), round(loc$y))
	names(coord) <- sliceDimNames
	if ( length(fixCoord) > 0 ) {
		for ( i in seq_along(fixCoord) ) {
			coord[names(fixCoord[[i]])] <- fixCoord[[i]]
		}
	}
	selected <- rep(FALSE, numPixels(object))
	selected[pixels(object, coord=coord)] <- TRUE
	as.vector(selected)
}
