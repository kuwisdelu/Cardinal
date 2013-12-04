
#### Returns a look-up array table for pixel coordinates ####
## 'coord' is a data.frame giving pixel coordinates
## 'dim' is the maximum extent of each dimension
##-----------------------------------------------
generatePositionArray <- function(coord, dim) {
	if ( nrow(coord) == 0 ) {
		dim <- rep(0, ncol(coord))
		names(dim) <- names(coord)
		return(array(1, dim=dim))
	}
	if ( missing(dim) ) dim <- sapply(coord, max, USE.NAMES=TRUE)
	positionArray <- array(1:prod(dim), dim=dim)
	fill <- apply(coord, 1,
		function(xyz) {
			do.call("[", c(list(positionArray), xyz))
		})
	positionArray <- array(NA, dim=dim)
	positionArray[fill] <- seq_len(nrow(coord))
	positionArray
}

