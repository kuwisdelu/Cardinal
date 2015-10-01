
#### Interpolationg for a 2D image ####

interpolate.bilinear <- function(x, xres=2, size=xres*dim(x), ...) {
	obj <- list(x=1:dim(x)[[1]], y=1:dim(x)[[2]], z=x)
	grid.list <- list(x=seq(1, dim(x)[[1]], length.out=size[[1]]),
		y=seq(1, dim(x)[[2]], length.out=size[[2]]))
	interp.surface.grid(obj, grid.list)$z
}

# copied from 'fields' package because 'maps' package is broken on Windows
interp.surface.grid <- function (obj, grid.list) {
	x <- grid.list$x
	y <- grid.list$y
	M <- length(x)
	N <- length(y)
	out <- matrix(NA, nrow = M, ncol = N)
	for (i in 1:M) {
	    out[i, ] <- interp.surface(obj, cbind(rep(x[i], N), y))
	}
	list(x = x, y = y, z = out)
}

# copied from 'fields' package because 'maps' package is broken on Windows
interp.surface <- function (obj, loc)  {
	x <- obj$x
	y <- obj$y
	z <- obj$z
	nx <- length(x)
	ny <- length(y)
	lx <- approx(x, 1:nx, loc[, 1])$y
	ly <- approx(y, 1:ny, loc[, 2])$y
	lx1 <- floor(lx)
	ly1 <- floor(ly)
	ex <- lx - lx1
	ey <- ly - ly1
	ex[lx1 == nx] <- 1
	ey[ly1 == ny] <- 1
	lx1[lx1 == nx] <- nx - 1
	ly1[ly1 == ny] <- ny - 1
	return(z[cbind(lx1, ly1)] * (1 - ex) * (1 - ey) + z[cbind(lx1 + 
	    1, ly1)] * ex * (1 - ey) + z[cbind(lx1, ly1 + 1)] * (1 - 
	    ex) * ey + z[cbind(lx1 + 1, ly1 + 1)] * ex * ey)
}
