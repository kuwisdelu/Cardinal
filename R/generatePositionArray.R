
#### Returns a look-up array table for pixel coordinates ####
## 'coord' is a data.frame giving pixel coordinates
## 'dim' is the maximum extent of each dimension
##-----------------------------------------------
generatePositionArray <- function(coord, dim, dimnames) {
	if ( nrow(coord) == 0 ) {
		dim <- rep(0, ncol(coord))
		names(dim) <- names(coord)
		return(array(0, dim=dim))
	}
	coord <- data.frame(lapply(coord, as.integer))
	if ( missing(dim) )
		dim <- sapply(coord, max)
	if ( missing(dimnames) )
		dimnames <- lapply(dim, seq_len)
	positionArray <- array(1:prod(dim), dim=dim)
	f <- function(...) positionArray[...]
	fill <- apply(coord, 1, function(xyz) do.call(f, as.list(xyz)))
	positionArray <- array(NA, dim=dim, dimnames=dimnames)
	positionArray[fill] <- seq_len(nrow(coord))
	positionArray
}

