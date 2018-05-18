
facet.plot <- function(args, formula, obj,
	facets, groups, superpose, strip, key, ...,
	xlab, xlim, ylab, ylim, layout,
	col, subset, add)
{
	call <- match.call()
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
	ys2 <- unlist(ys, use.names=FALSE)
	ysrange <- range(ys2, na.rm=TRUE)
	rx <- min(diff(sort(unique(x))), na.rm=TRUE)
	ry <- min(diff(sort(unique(ys2))), na.rm=TRUE)
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
	yrange <- ysrange
	if ( missing(xlim) )
		xlim <- xrange + rx * c(-0.5, 0.5)
	if ( missing(ylim) )
		ylim <- yrange + ry * c(-0.5, 0.5)
	if ( !missing(layout) )
		.setup.layout(layout)
	for ( f in facet_levels ) {
		facet_ids <- subrows(facets, f)
		for ( i in facet_ids ) {
			y <- ys[[i]]
			v <- names(ys)[i]
			if ( has_groups || superpose || !is.numeric(y) ) {
				if ( has_groups ) {
					levels <- levels(groups)
				} else if ( superpose ) {
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
				y <- as.numeric(y)
			for ( g in levels(groups) ) {
				gi <- groups
				xi <- x
				yi <- y
				yi[gi != g] <- NA
				if ( has_cats ) {
					if ( has_groups ) {
						cat <- g
					} else if ( superpose ) {
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
				if ( !all(is.na(yi)) ) {
					if ( add ) {
						points(xi, yi, ...,
							xlab=xlab, ylab=ylabi,
							xlim=xlim, ylim=ylim,
							col=coli)
					} else {
						plot(xi, yi, ...,
							xlab=xlab, ylab=ylabi,
							xlim=xlim, ylim=ylim,
							col=coli)
					}
				} else if ( add ) {
					points(0, 0, type='n', ...,
						xlab=xlab, ylab=ylab,
						xlim=xlim, ylim=ylim)
				} else {
					plot(0, 0, type='n', ...,
						xlab=xlab, ylab=ylab,
						xlim=xlim, ylim=ylim)
				}
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
				.draw.strip.labels(strip, text)
				if ( has_cats )
					.draw.key(key, levels, colors)
			}
			add <- superpose
		}
		add <- FALSE
	}
	invisible(call)
}

