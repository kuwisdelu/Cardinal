
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
	coord <- data.frame(lapply(coord, as.integer))
	if ( missing(dim) ) dim <- sapply(coord, max, USE.NAMES=TRUE)
	positionArray <- array(1:prod(dim), dim=dim)
	f <- function(...) positionArray[...]
	fill <- apply(coord, 1, function(xyz) do.call(f, as.list(xyz)))
	positionArray <- array(NA, dim=dim)
	positionArray[fill] <- seq_len(nrow(coord))
	positionArray
}

# pads and binds positionArrays along a new dimension
.positionArrayBind <- function(x, y) {
	y <- y + sum(!is.na(x))
	ldimx <- length(dim(x))
	ldimy <- length(dim(y))
	if ( ldimx < ldimy )
		dim(x) <- c(dim(x), rep(1, ldimy - ldimx))
	if ( ldimx > ldimy )
		dim(y) <- c(dim(y), rep(1, ldimx - ldimy))
	dims <- pmax(dim(x), dim(y))
	xind <- mapply(function(dx, dm) seq_len(dx)[seq_len(dm)], dim(x), dims,
		SIMPLIFY=FALSE, USE.NAMES=FALSE)
	yind <- mapply(function(dy, dm) seq_len(dy)[seq_len(dm)], dim(y), dims,
		SIMPLIFY=FALSE, USE.NAMES=FALSE)
	x <- do.call("[", c(list(x), xind))
	y <- do.call("[", c(list(y), yind))
	z <- c(x, y)
	dim(z) <- c(dims, 2)
	z
}
