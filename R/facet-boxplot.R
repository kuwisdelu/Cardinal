
facet.boxplot <- function(args, ...)
{
	x <- args$rhs[[1L]]
	if ( !is.factor(x) )
		args$rhs[[1L]] <- as.factor(x)
	out <- facet.plot(args, ...)
	class(out) <- "facet.boxplot"
	out
}

print.facet.boxplot <- function(x, ...) {
	obj <- .update.par(x, ...)
	if ( isTRUE(obj$layout$layout) ) {
		layout <- .auto.layout(obj,
			byrow=obj$layout$byrow, par=obj$par)
	} else if ( is.numeric(obj$layout$layout) ) {
		layout <- .setup.layout(obj$layout$layout,
			byrow=obj$layout$byrow, par=obj$par)
	} else {
		layout <- obj$layout
	}
	if ( isTRUE(obj$dark) || getOption("Cardinal.dark") ) {
		darkmode()
	} else if ( isFALSE(obj$dark) ) {
		lightmode()
	}
	if ( obj$add )
		.next.figure(last=TRUE)
	nil <- c(list(x=NA, y=NA), obj$par)
	rm <- names(formals(graphics::boxplot.default))
	rm <- rm[rm %in% names(obj$par)]
	nil[rm] <- NULL
	nil$type <- 'n'
	nil$xaxt <- 'n'
	for ( facet in obj$facets ) {
		for ( layer in facet ) {
			new <- !layer$add
			if ( !all(is.na(layer$x)) ) {
				args <- c(list(
					formula=y ~ x,
					data=list(x=layer$x, y=layer$y),
					add=TRUE, col=layer$col), obj$par)
			} else {
				args <- nil
			}
			if ( new ) {
				if ( obj$add ) {
					.next.figure(layout)
				} else {
					do.call("plot", nil)
					if ( isTRUE(obj$grid) ) grid()
					if ( !is.null(obj$preplot) ) {
						call <- obj$preplot$call
						e <- obj$preplot$envir
						eval(call, envir=e)
					}
					names <- levels(layer$x)
					if ( is.null(obj$par$xaxt) )
						axis(side=1, labels=names,
							at=seq_along(names))
				}
			}
			n <- length(obj$groups)
			nx <- 2 * (n %/% 2)
			i <- which(obj$groups %in% layer$group)
			nl <- nlevels(layer$x)
			grouping.factor <- 0.9
			if ( n %% 2 == 0 ) {
				d <- grouping.factor / nx
				dx <- (d / 2) + d * (n %/% 2 - 1)
			} else {
				d <- grouping.factor / (nx + 1)
				dx <- d * (n %/% 2)
			}
			at <- (seq_len(nl) - dx) + (d * (i - 1))
			if ( is.null(args$pars) )
				args$pars <- list()
			if ( is.null(args$pars$boxwex) ) {
				scale <- 0.8
			} else {
				scale <- args$pars$boxwex
			}
			args$xaxt <- 'n'
			args$pars$boxwex <- scale / n
			args$at <- at
			if ( isTRUE(obj$jitter) ) {
				args$outline <- FALSE
				do.call("boxplot", args)
				jargs <- c(list(
					x=jitter(as.integer(layer$x)),
					y=layer$y), obj$par)
				do.call("points", jargs)
			} else {
				do.call("boxplot", args)
			}
		}
		strip <- attr(facet, "strip")
		if ( !is.null(strip) )
			.draw.strip.labels(strip$strip, strip$text)
		key <- attr(facet, "key")
		if ( !is.null(key) )
			.draw.key(key$key, key$text, key$fill)
	}
	.Cardinal$lastplot <- x
	invisible(x)
}

