
#### Image plotting with a formula interface ####
## ---------------------------------------------

setMethod("image", c(x = "formula"),
	function(x, data = environment(x), ..., xlab, ylab, zlab, subset)
{
	args <- .parseFormula2(x, lhs.e=data, rhs.e=data, g.e=data)
	is3d <- length(args$rhs) == 3L
	if ( length(args$lhs) > 1L )
		.stop("lhs must include exactly 1 variable;",
			" did you mean to use I()?")
	if ( length(args$rhs) != 2L && length(args$rhs) != 3L )
		.stop("rhs of formula must include exactly 2 or 3 variables")
	if ( !is.null(args$g) )
		.stop("conditioning variables via | not allowed")
	e <- environment(x)
	x <- args$rhs[[1]]
	y <- args$rhs[[2]]
	if ( is3d ) {
		z <- args$rhs[[3]]
	} else {
		z <- NULL
	}
	vals <- args$lhs[[1]]
	if ( !missing(subset) ) {
		sub <- eval(substitute(subset, parent.frame(2)),
			envir=data, enclos=e)
		if ( is.logical(sub) )
			sub <- rep_len(sub, length(vals))
		x <- x[sub]
		y <- y[sub]
		z <- z[sub]
		vals <- vals[sub]
	}
	if ( length(x) != length(vals) || length(y) != length(vals) )
		.stop("variable lengths differ")
	if ( is3d && length(z) != length(vals) )
		.stop("variable lengths differ")
	coord <- data.frame(x=x, y=y)
	coord$z <- z
	res <- .estimateSpatialResolution(coord)
	gridded <- all(is.finite(res))
	if ( gridded ) {
		dim <- .getDimsFromResolution(coord, res)
		rx <- res[1]
		ry <- res[2]
		if ( !is.null(z) )
			rz <- res[3]
	} else {
		dim <- NULL
		rx <- min(diff(sort(unique(x))), na.rm=TRUE)
		ry <- min(diff(sort(unique(y))), na.rm=TRUE)
		if ( !is.null(z) )
			rz <- min(diff(sort(unique(z))), na.rm=TRUE)
	}
	if ( missing(xlab) )
		xlab <- names(args$rhs)[1]
	if ( missing(ylab) )
		ylab <- names(args$rhs)[2]
	if ( is3d && missing(zlab) )
		zlab <- names(args$rhs)[3]
	if ( !is3d ) {
		xrange <- range(x, na.rm=TRUE)
		yrange <- range(y, na.rm=TRUE)
		z <- projectToRaster(x, y, vals, dim=dim)
		x <- seq(xrange[1], xrange[2], length.out=dim(z)[1])
		y <- seq(yrange[1], yrange[2], length.out=dim(z)[2])
		image(x, y, z, ..., xlab=xlab, ylab=ylab)
	} else {
		points3d(x, y, z, vals, ..., xlab=xlab, ylab=ylab, zlab=zlab)
	}
})

projectToRaster <- function(x, y, values, dim = NULL, res = NULL) {
	xrange <- range(x, na.rm=TRUE)
	yrange <- range(y, na.rm=TRUE)
	if ( is.null(dim) ) {
		asp <- diff(yrange) / diff(xrange)
		nrow <- as.integer(sqrt(length(values) / asp))
		ncol <- as.integer(asp * nrow)
	} else {
		nrow <- dim[1]
		ncol <- dim[2]
	}
	if ( is.null(res) || is.na(res[1]) ) {
		rx <- (xrange[2] - xrange[1]) / (nrow - 1)
	} else {
		rx <- res[1]
	}
	if ( is.null(res) || is.na(res[2]) ) {
		ry <- (yrange[2] - yrange[1]) / (ncol - 1)
	} else {
		ry <- res[2]
	}
	rx <- ifelse(is.finite(rx), rx, 1)
	ry <- ifelse(is.finite(ry), ry, 1)
	rows <- as.integer(round((x - xrange[1]) / rx))
	cols <- as.integer(round((y - yrange[1]) / ry))
	init <- as.vector(NA, mode=typeof(values))
	rs <- matrix(init, nrow=nrow, ncol=ncol)
	idx <- rows + cols * nrow + 1L
	valid <- is.finite(idx)
	values <- values[valid]
	idx <- idx[valid]
	idx[idx > length(rs)] <- length(rs)
	rs[idx] <- values
	rs
}

projectToRaster3d <- function(x, y, z, values, dim = NULL, res = NULL) {
	xrange <- range(x, na.rm=TRUE)
	yrange <- range(y, na.rm=TRUE)
	zrange <- range(z, na.rm=TRUE)
	if ( is.null(dim) ) {
		axy <- diff(yrange) / diff(xrange)
		axz <- diff(zrange) / diff(xrange)
		ayz <- diff(zrange) / diff(yrange)
		nx <- as.integer(cbrt(length(values) / (axz * axy)))
		ny <- as.integer((axz / ayz) * nx)
		nz <- as.integer((ayz * axy) * nx)
	} else {
		nx <- dim[1]
		ny <- dim[2]
		nz <- dim[3]
	}
	if ( is.null(res) || is.na(res[1]) ) {
		rx <- (xrange[2] - xrange[1]) / (nx - 1)
	} else {
		rx <- res[1]
	}
	if ( is.null(res) || is.na(res[2]) ) {
		ry <- (yrange[2] - yrange[1]) / (ny - 1)
	} else {
		ry <- res[2]
	}
	if ( is.null(res) || is.na(res[3]) ) {
		rz <- (zrange[2] - zrange[1]) / (nz - 1)
	} else {
		rz <- res[3]
	}
	rx <- ifelse(is.finite(rx), rx, 1)
	ry <- ifelse(is.finite(ry), ry, 1)
	rz <- ifelse(is.finite(rz), rz, 1)
	ix <- as.integer(round((x - xrange[1]) / rx))
	iy <- as.integer(round((y - yrange[1]) / ry))
	iz <- as.integer(round((z - zrange[1]) / rz))
	init <- as.vector(NA, mode=typeof(values))
	rs <- array(init, dim=c(nx, ny, nz))
	idx <- ix + iy * nx + iz * nx * ny + 1L
	idx[idx > length(rs)] <- length(rs)
	rs[idx] <- values
	rs
}

