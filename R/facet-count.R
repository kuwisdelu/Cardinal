
facet.count <- function(args, formula, obj,
	facets, groups, superpose, strip, key, probability, ...,
	xlab, xlim, ylab, ylim, layout, byrow, dark,
	col, grid, subset, preplot, add)
{
	dots <- list(...)
	e <- environment(formula)
	xs <- args$rhs
	is_discrete <- all(sapply(xs, is.discrete))
	n <- nrow(obj)
	if ( superpose && !is.null(groups) )
		.stop("cannot specify 'superpose' and 'groups' in same call")
	if ( !is.null(groups) ) {
		has_groups <- TRUE
	} else {
		groups <- rep_len(factor(1), n)
		has_groups <- FALSE
	}
	if ( !is.null(subset) ) {
		xs <- lapply(xs, "[", subset)
		groups <- groups[subset]
	}
	if ( any(lengths(xs) == 1L) )
		.stop("can't estimate reasonable axes")
	if ( !is.null(facets) ) {
		if ( !is.data.frame(facets) )
			facets <- as.data.frame(facets)
		facets[] <- lapply(facets, function(fc) {
			if ( is.factor(fc) ) {
				droplevels(fc)
			} else {
				factor(fc, levels=unique(fc))
			}
		})
		has_facets <- TRUE
	} else {
		facets <- rep_len(factor(1), length(xs))
		facets <- as.data.frame(facets)
		has_facets <- FALSE
	}
	facet_levels <- unique(facets)
	facet_levels <- lapply(seq_len(nrow(facet_levels)),
		function(i) facet_levels[i,,drop=FALSE])
	if ( missing(xlab) ) {
		if ( length(unique(names(args$rhs))) != 1L ) {
			xlab <- character(1)
		} else {
			xlab <- unique(names(args$rhs))
		}
	}
	if ( missing(probability) || is.null(probability) )
		probability <- FALSE
	if ( missing(ylab) ) {
		if ( probability ) {
			if ( is_discrete ) {
				ylab <- "proportion"
			} else {
				ylab <- "density"
			}
		} else {
			ylab <- "count"
		}
	}
	hformals <- names(formals(graphics::hist.default))
	xrange <- c(NA, NA)
	yrange <- c(NA, NA)
	plotnew <- !add
	add <- FALSE
	facets.out <- list()
	for ( f in facet_levels ) {
		facet_ids <- subset_rows(facets, f)
		for ( i in facet_ids ) {
			x <- xs[[i]]
			v <- names(xs)[i]
			if ( has_groups || superpose ) {
				if ( has_groups ) {
					levels <- levels(groups)
				} else if ( superpose ) {
					levels <- na.omit(unique(names(xs)))
				} else {
					levels <- NULL
				}
				nlevels <- length(levels)
				if ( is.function(col) ) {
					colors <- col(nlevels)
				} else {
					colors <- col
				}
				if ( length(colors) != nlevels )
					colors <- rep_len(colors, nlevels)
				has_cats <- TRUE
			} else {
				if ( is.function(col) ) {
					colors <- col(1)
				} else {
					colors <- col
				}
				has_cats <- FALSE
			}
			if ( !is.numeric(x) )
				x <- as.factor(x)
			layers <- list()
			for ( g in levels(groups) ) {
				gi <- groups
				data <- x[gi == g]
				if ( has_cats ) {
					if ( has_groups ) {
						cat <- g
					} else if ( superpose || key ) {
						cat <- v
					} else {
						cat <- NULL
					}
					coli <- setNames(colors, levels)
					coli <- coli[cat]
				} else {
					coli <- colors
				}
				if ( length(xlab) > 1L ) {
					xlabi <- xlab[i]
				} else {
					xlabi <- xlab
				}
				if ( is_discrete ) {
					class <- levels(data)
					count <- as.numeric(table(data))
					proportion <- count / sum(count, na.rm=TRUE)
					xrange <- range(c(xrange, seq_along(class)), na.rm=TRUE)
					if ( probability ) {
						yrange <- range(c(yrange, proportion), na.rm=TRUE)
					} else {
						yrange <- range(c(yrange, count), na.rm=TRUE)
					}
					layers[[length(layers) + 1L]] <- list(
						class=class, count=count, prop=proportion,
						col=coli, facet=f, group=g, add=add)
				} else {
					hargs <- dots[names(dots) %in% hformals]
					stat <- do.call("hist", c(list(x=data, plot=FALSE), hargs))
					xrange <- range(c(xrange, stat$breaks), na.rm=TRUE)
					if ( probability ) {
						yrange <- range(c(yrange, stat$density), na.rm=TRUE)
					} else {
						yrange <- range(c(yrange, stat$count), na.rm=TRUE)
					}
					layers[[length(layers) + 1L]] <- list(
						breaks=stat$breaks, mids=stat$mids,
						count=stat$count, density=stat$density,
						col=coli, facet=f, group=g, add=add)
				}
				add <- TRUE
			}
			last <- i == max(facet_ids)
			if ( !superpose || last ) {
				text <- character()
				if ( length(xs) > 1L || has_facets ) {
					if ( has_facets ) {
						text <- c(sapply(f, as.character), text)
					} else if ( !superpose ) {
						text <- c(v, text)
					}
				}
				attr(layers, "strip") <- list(
					strip=strip, text=text)
				if ( has_cats ) {
					attr(layers, "key") <- list(
						key=key, text=levels, fill=colors)
				}
			}
			facets.out <- c(facets.out, list(layers))
			add <- superpose
		}
		add <- FALSE
	}
	rm <- hformals
	rm <- rm[rm %in% names(dots)]
	dots[rm] <- NULL
	if ( is_discrete ) {
		rx <- 1
	} else {
		rx <- 0.1 * diff(xrange)
	}
	ry <- 0.1 * diff(yrange)
	if ( missing(layout) )
		layout <- TRUE
	if ( missing(byrow) )
		byrow <- TRUE
	layout <- list(layout=layout, byrow=byrow)
	if ( missing(preplot) )
		preplot <- NULL
	if ( missing(xlim) || is.null(xlim) )
		xlim <- xrange + rx * c(-0.5, 0.5)
	if ( missing(ylim) || is.null(ylim) )
		ylim <- c(0, yrange[2] + ry * 0.5)
	par <- list(
		xlab=xlab, ylab=ylab,
		xlim=xlim, ylim=ylim)
	out <- list(
		facets=facets.out,
		flevels=facet_levels,
		groups=levels(groups),
		subset=subset,
		probability=probability,
		layout=layout,
		grid=grid, preplot=preplot,
		add=!plotnew, par=c(par, dots))
	if ( !missing(dark) )
		out$dark <- dark
	if ( is_discrete ) {
		class(out) <- "facet.bar"
	} else {
		class(out) <- "facet.hist"
	}
	out
}

print.facet.bar <- function(x, ...) {
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
	nil$type <- 'n'
	nil$xaxt <- 'n'
	nil$yaxs <- 'i'
	for ( facet in obj$facets ) {
		for ( layer in facet ) {
			new <- !layer$add
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
					names <- layer$class
					if ( is.null(obj$par$xaxt) )
						axis(side=1, labels=names,
							at=seq_along(names))
				}
			}
			n <- length(obj$groups)
			nx <- 2 * (n %/% 2)
			i <- which(obj$groups %in% layer$group)
			nl <- length(layer$class)
			grouping.factor <- 0.9
			if ( n %% 2 == 0 ) {
				d <- grouping.factor / nx
				dx <- (d / 2) + d * (n %/% 2 - 1)
			} else {
				d <- grouping.factor / (nx + 1)
				dx <- d * (n %/% 2)
			}
			at <- (seq_len(nl) - dx) + (d * (i - 1))
			scale <- 0.8
			width <- scale / n
			if ( obj$probability ) {
				yi <- layer$prop
			} else {
				yi <- layer$count
			}
			args <- c(list(
				xleft=at - (width / 2),
				xright=at + (width / 2),
				ybottom=0, ytop=yi,
				col=layer$col), obj$par)
			do.call("rect", args)
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



print.facet.hist <- function(x, ...) {
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
	nil$type <- 'n'
	nil$yaxs <- 'i'
	for ( facet in obj$facets ) {
		for ( layer in facet ) {
			new <- !layer$add
			if ( obj$probability ) {
				args <- c(list(
					x=layer$breaks, y=layer$density,
					col=layer$col), obj$par)
			} else {
				args <- c(list(
					x=layer$breaks, y=layer$count,
					col=layer$col), obj$par)
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
				}
			}
			a1 <- a2 <- args
			a1$y <- c(0, a1$y)
			a1$type <- "S"
			a2$y <- c(a2$y, 0)
			a2$type <- "s"
			do.call("lines", a1)
			do.call("lines", a2)
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

