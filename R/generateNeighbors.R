
#### Returns a list of look-up array tables for pixel neighbors ####
## 'coord' is a data.frame giving pixel coordinates
## 'r' is neighborhood radius
## 'coordLabels' indicates which dimensions to consider
## 'na.rm' means out-of-bounds elements are replaced with the center index
## 'simplify' simplifies to a (2 * r + 1)^2 x N matrix
##-----------------------------------------------
generateNeighbors <- function(coord, r, coordLabels, na.rm=FALSE) {
	if ( r == 0 )
		return(matrix(seq_len(nrow(coord)), ncol=nrow(coord)))
	if ( missing(coordLabels) )
		coordLabels <- names(coord)
	coord <- data.frame(lapply(coord, as.integer))
	dim <- sapply(coord, max)
	positionArray <- generatePositionArray(coord + r, dim + (2 * r))
	coord <- coord + r
	f <- function(...) positionArray[...]
	radii <- rep(list((-r):r), ncol(coord))
	names(radii) <- names(coord)
	radii[!names(coord) %in% coordLabels] <- 0
	lapply(seq_len(nrow(coord)), function(i) {
		neighbors <- do.call(f, mapply(`+`, coord[i,], radii, SIMPLIFY=FALSE))
		if ( na.rm ) neighbors[is.na(neighbors)] <- i
		dimnames(neighbors) <- radii[coordLabels]
		neighbors
	})
}
