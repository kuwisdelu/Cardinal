
facet.raster <- function(rasters,
	offsets, heights, widths, strip, ...,
	xlab, ylab, xaxs, yaxs, xlim, ylim, asp, margin,
	layout, byrow, dark, interpolate, add)
{
	dots <- list(...)
	if ( !is.list(rasters) )
		rasters <- list(rasters)
	if ( !is.list(offsets) )
		offsets <- list(offsets)
	facets <- mapply(function(raster, offset, height, width) {
		xleft <- offset[1]
		xright <- offset[1] + width
		ytop <- offset[2]
		ybottom <- offset[2] + height
		facet <- list(raster=raster,
			xleft=xleft, xright=xright,
			ybottom=ybottom, ytop=ytop)
	}, rasters, offsets, heights, widths, SIMPLIFY=FALSE)
	names(facets) <- names(rasters)
	if ( !is.null(names(facets)) )
		facets <- mapply(function(facet, text) {
			attr(facet, "strip") <- list(
				strip=strip, text=text)
			facet
		}, facets, names(facets), SIMPLIFY=FALSE)
	xmin <- min(sapply(facets, function(x) x$xleft))
	xmax <- max(sapply(facets, function(x) x$xright))
	ymin <- min(sapply(facets, function(x) x$ytop))
	ymax <- max(sapply(facets, function(x) x$ybottom))
	if ( missing(xlab) )
		xlab <- ""
	if ( missing(ylab) )
		ylab <- ""
	if ( missing(xaxs) )
		xaxs <- "i"
	if ( missing(yaxs) )
		yaxs <- "i"
	if ( missing(layout) )
		layout <- TRUE
	if ( missing(byrow) )
		byrow <- TRUE
	layout <- list(layout=layout, byrow=byrow)
	if ( missing(margin) )
		margin <- 0
	if ( missing(xlim) || is.null(xlim) )
		xlim <- c(xmin - margin, xmax + margin)
	if ( missing(ylim) || is.null(ylim) )
		ylim <- c(ymin - margin, ymax + margin)
	if ( missing(asp) )
		asp <- 1
	par <- list(
		xlab=xlab, ylab=ylab,
		xlim=xlim, ylim=ylim,
		xaxs=xaxs, yaxs=yaxs,
		asp=asp)
	out <- list(
		facets=facets,
		layout=layout,
		interpolate=interpolate,
		add=add, par=c(par, dots))
	class(out) <- "facet.raster"
	out
}

print.facet.raster <- function(x, ...) {
	obj <- .update.par(x, ...)
	if ( isTRUE(obj$dark) || getOption("Cardinal.dark", default=FALSE) ) {
		darkmode(default=FALSE)
	} else if ( isFALSE(obj$dark) ) {
		lightmode(default=FALSE)
	}
	if ( isTRUE(obj$layout$layout) ) {
		layout <- .auto.layout(length(obj$facets),
			byrow=obj$layout$byrow, par=obj$par)
	} else if ( is.numeric(obj$layout$layout) ) {
		layout <- .setup.layout(obj$layout$layout,
			byrow=obj$layout$byrow, par=obj$par)
	} else {
		layout <- obj$layout
	}
	obj$par$ylim <- rev(obj$par$ylim)
	nil <- c(list(x=NA, y=NA), obj$par)
	nil$type <- 'n'
	for ( facet in obj$facets ) {
		if ( obj$add ) {
			.next.figure(layout)
		} else {
			do.call("plot", nil)
		}
		rasterImage(facet$raster,
			xleft=facet$xleft, xright=facet$xright,
			ybottom=facet$ybottom, ytop=facet$ytop,
			interpolate=obj$interpolate)
		strip <- attr(facet, "strip")
		if ( !is.null(strip) )
			.draw.strip.labels(strip$strip, strip$text)
	}
	.Cardinal$lastplot <- x
	invisible(x)
}

