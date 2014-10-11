
#### Interpolationg for a 2D image ####

interpolate.bilinear <- function(x, xres=2, size=xres*dim(x), ...) {
	obj <- list(x=1:dim(x)[[1]], y=1:dim(x)[[2]], z=x)
	grid.list <- list(x=seq(1, dim(x)[[1]], length.out=size[[1]]),
		y=seq(1, dim(x)[[2]], length.out=size[[2]]))
	interp.surface.grid(obj, grid.list)$z
}
