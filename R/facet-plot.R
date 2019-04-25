
facet.plot <- function(args, formula, obj,
	facets, groups, superpose, strip, key, ...,
	xlab, xlim, ylab, ylim, layout, dark, col,
	subset, preplot, add)
{
	dots <- list(...)
	e <- environment(formula)
	x <- args$rhs[[1]]
	ys <- args$lhs
	n <- nrow(obj)
	if ( any(lengths(ys) != n) || length(x) != n )
		.stop("variable lengths differ")
	if ( superpose && !is.null(groups) )
		.stop("cannot specify 'superpose' and 'groups' in same call")
	if ( !is.null(groups) ) {
		has_groups <- TRUE
	} else {
		groups <- rep_len(factor(1), n)
		has_groups <- FALSE
	}
	if ( !is.null(subset) ) {
		x <- x[subset]
		ys <- lapply(ys, "[", subset)
		groups <- groups[subset]
	}
	if ( length(x) == 1L )
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
		facets <- rep_len(factor(1), length(ys))
		facets <- as.data.frame(facets)
		has_facets <- FALSE
	}
	facet_levels <- unique(facets)
	facet_levels <- lapply(seq_len(nrow(facet_levels)),
		function(i) facet_levels[i,,drop=FALSE])
	raw_ys <- unlist(ys, use.names=FALSE)
	if ( is.numeric(raw_ys) ) {
		raw_ysrange <- range(raw_ys, na.rm=TRUE)
	} else {
		raw_ysrange <- c(NA, NA)
	}
	rx <- min(diff(sort(unique(x))), na.rm=TRUE)
	ry <- min(diff(sort(unique(raw_ys))), na.rm=TRUE)
	if ( missing(xlab) )
		xlab <- names(args$rhs)[1]
	if ( missing(ylab) ) {
		if ( length(unique(names(args$lhs))) != 1L ) {
			ylab <- character(1)
		} else {
			ylab <- unique(names(args$lhs))
		}
	}
	xrange <- range(x, na.rm=TRUE)
	yrange <- raw_ysrange
	layers <- list()
	for ( f in facet_levels ) {
		facet_ids <- subset_rows(facets, f)
		for ( i in facet_ids ) {
			y <- ys[[i]]
			v <- names(ys)[i]
			if ( has_groups || superpose || !is.numeric(y) ) {
				if ( has_groups ) {
					levels <- levels(groups)
				} else if ( superpose  ) {
					levels <- na.omit(unique(names(ys)))
				} else {
					levels <- na.omit(unique(y))
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
			if ( !is.numeric(y) )
				y <- as.factor(y)
			sublayers <- list()
			for ( g in levels(groups) ) {
				gi <- groups
				xi <- x[gi == g]
				yi <- y[gi == g]
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
				if ( length(ylab) > 1L ) {
					ylabi <- ylab[i]
				} else {
					ylabi <- ylab
				}
				sublayers[[length(sublayers) + 1L]] <- list(
					x=xi, y=yi, col=coli, facet=f, group=g, add=add)
				add <- TRUE
			}
			last <- i == max(facet_ids)
			if ( !superpose || last ) {
				text <- character()
				if ( length(ys) > 1L || has_facets ) {
					if ( has_facets ) {
						text <- c(sapply(f, as.character), text)
					} else if ( !superpose ) {
						text <- c(v, text)
					}
				}
				attr(sublayers, "strip") <- list(
					strip=strip, text=text)
				if ( has_cats ) {
					attr(sublayers, "key") <- list(
						key=key, text=levels, fill=colors)
				}
			}
			layers <- c(layers, list(sublayers))
			add <- superpose
		}
		add <- FALSE
	}
	if ( missing(layout) )
		layout <- TRUE
	if ( missing(preplot) )
		preplot <- NULL
	if ( missing(xlim) || is.null(xlim) )
		xlim <- xrange + rx * c(-0.5, 0.5)
	if ( missing(ylim) || is.null(ylim) )
		ylim <- yrange + ry * c(-0.5, 0.5)
	par <- list(
		xlab=xlab, ylab=ylab,
		xlim=xlim, ylim=ylim)
	out <- list(
		layers=layers,
		facets=facet_levels,
		groups=levels(groups),
		subset=subset,
		layout=layout,
		preplot=preplot,
		par=c(par, dots))
	if ( !missing(dark) )
		out$dark <- dark
	class(out) <- "facet.plot"
	out
}

print.facet.plot <- function(x, ...) {
	obj <- x
	if ( isTRUE(obj$layout) ) {
		.auto.layout(obj)
	} else if ( is.numeric(obj$layout) ) {
		.setup.layout(obj$layout)
	}
	if ( isTRUE(obj$dark) || getOption("Cardinal.dark") ) {
		darkmode()
	} else if ( isFALSE(obj$dark) ) {
		lightmode()
	}
	nil <- c(list(x=0, y=0), obj$par)
	nil$type <- 'n'
	for ( layer in obj$layers ) {
		for ( sublayer in layer ) {
			if ( !all(is.na(sublayer$x)) ) {
				args <- c(list(
					x=sublayer$x, y=sublayer$y,
					col=sublayer$col), obj$par)
			} else {
				args <- nil
			}
			new <- !sublayer$add
			if ( new ) {
				do.call("plot", nil)
				if ( !is.null(obj$preplot) ) {
					call <- obj$preplot$call
					e <- obj$preplot$envir
					eval(call, envir=e)
				}
			}	
			do.call("points", args)
		}
		strip <- attr(layer, "strip")
		if ( !is.null(strip) )
			.draw.strip.labels(strip$strip, strip$text)
		key <- attr(layer, "key")
		if ( !is.null(key) )
			.draw.key(key$key, key$text, key$fill)
	}
	.Cardinal$lastplot <- x
	invisible(x)
}

