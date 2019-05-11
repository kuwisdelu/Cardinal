
print.facet.raster <- function(x, ...) {
	obj <- .update.par(x, ...)
	if ( getOption("Cardinal.dark") )
		darkmode()
	if ( isTRUE(obj$layout$layout) ) {
		layout <- .auto.layout(length(obj$facets),
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
	for ( layer in obj$facets ) {
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

