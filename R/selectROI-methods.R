
# #### implement methods for selecting pixels ####

# setMethod("selectROI", "MSImageSet", function(object,
# 	sliceDimNames=metaData(object)[["coordDimNames"]][c(1,2)],
# 	fixCoord=list(), silent=FALSE, ...)
# {
# 	if ( length(sliceDimNames) != 2 | !is.character(sliceDimNames) ) {
# 		stop("'sliceDimNames' must list the name of 2 dimensions in 'coordDimNames'")	
# 	}
# 	if ( !silent ) message("Select pixels and press ESC or second mouse button when done")
# 	loc <- locator(type="o", pch=20, col="white", lwd=1.5)
# 	if ( is.null(loc) ) stop("no pixels selected")
# 	return(selectROIHelper(loc, object, sliceDimNames, fixCoord))
# } )

# #### helper functions ####

# selectROIHelper <- function(loc, object, sliceDimNames, fixCoord) {
# 	coord <- coord(object)[,sliceDimNames]
# 	selected <- point.in.polygon(coord[,1], coord[,2], loc$x, loc$y)
# 	selected <- selected > 0
# 	if ( length(fixCoord) > 0 ) {
# 		fixed <- sapply(seq_along(fixCoord), function(i) {
# 			coord(object)[names(fixCoord)[i]] == fixCoord[[i]]
# 		} )
# 	} else {
# 		fixed <- rep(TRUE, numPixels(object))
# 	}
# 	if ( length(fixCoord) > 1 ) {
# 		fixed <- apply(fixed, 1, all)
# 	}
# 	as.vector(selected & fixed)
# }
