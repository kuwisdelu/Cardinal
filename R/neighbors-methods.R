
#### implement neighbors methods ####

setMethod("neighbors", c("data.frame", "numeric"), function(object, r, which,
	autoDimNames=NULL, na.replace=FALSE, ...)
{
	if ( r == 0 ) return(as.matrix(seq_len(nrow(object))))
	if ( missing(which) ) which <- rep(TRUE, nrow(object))
	dim <- sapply(object, max)
	coord <- object[which,]
	# in case any are on the edge then make a positionArray buffered by NAs on the edges
	positionArray <- generatePositionArray(object+r, dim+2*r)
	coord <- coord+r
	f <- function(...) positionArray[...]
	rs <- rep(list((-r):r), ncol(object))
	rs[names(object) %in% autoDimNames] <- 0
	neighbors <- apply(coord, 1, function(xyz) as.numeric(do.call(f,
		mapply(`+`, xyz, rs, SIMPLIFY=FALSE))))
	center <- 2*r^2 + 2*r + 1
	if ( na.replace ) {
		for ( i in 1:ncol(neighbors) ) neighbors[is.na(neighbors[,i]),i] <- i
	}
	return(t(neighbors))
} )

setMethod("neighbors", c("matrix", "numeric"), function(object, r, which,
	autoDimNames=NULL, na.replace=FALSE, ...)
{
	if ( missing(which) ) which <- rep(TRUE, nrow(object))
	object <- as.data.frame(object)
	neighbors(object, r=r, which=which, autoDimNames=autoDimNames,
		na.replace=na.replace)
} )

setMethod("neighbors", c("MSImageSet", "numeric"), function(object, r, pixel, coord,
	autoDimNames=NULL, na.replace=FALSE, ...)
{
	pixel <- getPixels(object, pixel, coord)
	coord <- Cardinal:::coord(object)[pixel,]
	neighbors(coord(object), r=r, which=pixel, autoDimNames=autoDimNames,
		na.replace=na.replace)
} )


