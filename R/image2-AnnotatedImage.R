
#### Image plotting with a optical images ####
## ---------------------------------------------

setMethod("image", c(x = "AnnotatedImage"),
	function(x, frame = 1, offset = coord(x), height, width,
		xlab, ylab, xlim = NULL, ylim = NULL, xaxs = 'i', yaxs = 'i',
		native = TRUE, interpolate = TRUE, add = FALSE, ...)
{
	dots <- list(...)
	if ( missing(height) )
		height <- Cardinal::height(x)
	if ( missing(width) )
		width <- Cardinal::width(x)
	xleft <- offset[1]
	xright <- offset[1] + width
	ytop <- offset[2]
	ybottom <- offset[2] + height
	margin <- 0
	if ( is.null(xlim) )
		xlim <- c(xleft - margin, xright + margin)
	if ( is.null(ylim) )
		ylim <- c(ytop - margin, ybottom + margin)
	if ( missing(xlab) )
		xlab <- ""
	if ( missing(ylab) )
		ylab <- ""
	raster <- getFrame(x, frame, type="render")
	if ( native ) {
		raster <- EBImage:::as.nativeRaster(raster)
	} else {
		raster <- as.raster(raster)
	}
	raster <- list(
		raster=raster,
		xleft=xleft, xright=xright,
		ybottom=ybottom, ytop=ytop)
	par <- list(
		xlab=xlab, ylab=ylab,
		xlim=xlim, ylim=ylim,
		xaxs=xaxs, yaxs=yaxs)
	out <- list(
		layers=list(raster),
		interpolate=interpolate,
		add=add, par=c(par, dots))
	class(out) <- "raster.image"
	out
})

print.raster.image <- function(x, ...) {
	obj <- .update.par(x, ...)
	if ( getOption("Cardinal.dark") )
		darkmode()
	if ( isTRUE(obj$layout$layout) ) {
		layout <- .auto.layout(length(obj$layers),
			byrow=obj$layout$byrow)
	} else if ( is.numeric(obj$layout$layout) ) {
		layout <- .setup.layout(obj$layout$layout,
			byrow=obj$layout$byrow)
	} else {
		layout <- obj$layout
	}
	obj$par$asp <- 1
	obj$par$ylim <- rev(obj$par$ylim)
	nil <- c(list(x=NA, y=NA), obj$par)
	nil$type <- 'n'
	for ( layer in obj$layers ) {
		if ( obj$add ) {
			.next.figure(layout)
			par <- obj$par
			if ( "type" %in% names(par) )
				par <- par[-which(names(par) == "type")]
			do.call("plot.window", par)
		} else {
			do.call("plot", nil)
		}
		rasterImage(layer$raster,
			xleft=layer$xleft, xright=layer$xright,
			ybottom=layer$ybottom, ytop=layer$ytop,
			interpolate=obj$interpolate)
	}
	.Cardinal$lastplot <- x
	invisible(x)
}

