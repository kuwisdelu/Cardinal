
#### Create a 3D image ####

image3d <- function(
	x = seq(0, 1, length.out=dim(values)[1]),
	y = seq(0, 1, length.out=dim(values)[2]),
	z = seq(0, 1, length.out=dim(values)[3]),
	values,
	xlim = range(x),
	ylim = range(y),
	zlim = range(z),
	xlab, ylab, zlab,
	col = intensity.colors(100),
	alpha.power = 2,
	alpha = (seq_len(length(col))/length(col))^alpha.power,
	pch = 15, cex = 1,
	add = FALSE,
	...)
{
	if ( missing(values) ) {
		if ( !missing(x) ) {
			if ( is.list(x) ) {
				values <- x$values
				z <- x$z
				y <- x$y
				x <- x$x
			} else {
				if ( is.null(dim(x)) )
					stop("argument must be matrix-like")
				values <- x
				x <- seq.int(0, 1, length.out = dim(values)[1])
			}
			if ( missing(xlab) )
				xlab <- ""
			if ( missing(ylab) )
				ylab <- ""
			if ( missing(zlab) )
				zlab <- ""
		} else {
			stop("no 'values' array specified")
		}
    } else {
    	if ( missing(xlab) )
    		xlab <- if ( missing(x) ) "" else deparse(substitute(x))
		if ( missing(ylab) )
    		ylab <- if ( missing(y) ) "" else deparse(substitute(y))
		if ( missing(zlab) )
    		zlab <- if ( missing(z) ) "" else deparse(substitute(z))
    }
    if ( !add || is.null(.Cardinal$trans3d) )
    	.Cardinal$trans3d <- persp(xlim, ylim, matrix(zlim, nrow=2, ncol=2),
			xlim=xlim, ylim=ylim, zlim=zlim,
			xlab=xlab, ylab=ylab, zlab=zlab,
			border=NA, shade=NA, col=NA, ...)
	col <- alpha.colors(col=col, alpha=alpha)
	bins <- cut(values, breaks=seq(
		from=min(values, na.rm=TRUE),
		to=max(values, na.rm=TRUE),
		length.out=length(col)+1),
		include.lowest=TRUE)
	col <- col[bins]
	coord <- expand.grid(x=x, y=y, z=z)
	points(trans3d(coord$x, coord$y, coord$z, .Cardinal$trans3d),
		col=col, pch=pch, cex=cex)
	invisible(.Cardinal$trans3d)
}
