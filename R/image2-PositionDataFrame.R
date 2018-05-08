
#### Image plotting for PositionDataFrame ####
## ------------------------------------------

setMethod("image", c(x = "PositionDataFrame"),
	function(x, formula,
		groups = NULL,
		superpose = FALSE,
		strip = TRUE,
		key = superpose || !is.null(groups),
		normalize.image = c("none", "linear"),
		contrast.enhance = c("none", "suppression", "histogram"),
	    smooth.image = c("none", "gaussian", "adaptive"),
	    ...,
		xlab, xlim,
		ylab, ylim,
		zlab, zlim,
		asp = 1,
		layout,
		col = discrete.colors,
		colorscale = gradient.colors,
		colorkey = !is3d,
		subset = TRUE,
		add = FALSE)
{
	if ( missing(formula) ) {
		fm <- paste0("~", paste0(coordnames(x)[c(1,2)], collapse="*"))
		formula <- as.formula(fm, env=parent.frame(2))
	}
	e <- environment(formula)
	args <- .parseFormula2(formula,
		lhs.e=as.env(x, enclos=e),
		rhs.e=as.list(coord(x)))
	is3d <- length(args$rhs) == 3L
	if ( length(args$rhs) != 2L && length(args$rhs) != 3L )
		stop("rhs of formula must include exactly 2 or 3 variables")
	if ( !is.null(args$g) )
		stop("conditioning variables via | not allowed")
	if ( !missing(groups) ) {
		groups <- eval(substitute(groups), envir=as.env(x, enclos=e))
		if ( !is.factor(groups) ) {
			groups <- factor(groups, levels=unique(groups))
		} else {
			groups <- droplevels(groups)
		}
		if ( length(groups) != nrow(x) )
			groups <- rep_len(groups, nrow(x))
	}
	if ( !missing(subset) ) {
		subset <- eval(substitute(subset), envir=as.env(x, enclos=e))
		if ( is.logical(subset) )
			subset <- rep_len(subset, nrow(x))
	}
	facet.image(args, formula=formula, obj=x,
		facets=NULL, groups=groups,
		superpose=superpose,
		strip=strip, key=key,
		normalize.image=normalize.image,
		contrast.enhance=contrast.enhance,
		smooth.image=smooth.image, ...,
		xlab=xlab, xlim=xlim,
		ylab=ylab, ylim=ylim,
		zlab=zlab, zlim=zlim,
		asp=asp, layout=layout,
		col=col, colorscale=colorscale,
		colorkey=colorkey,
		subset=subset, add=add)
})

facet.image <- function(args, formula, obj,
	facets, groups, superpose, strip, key,
	normalize.image, contrast.enhance, smooth.image, ...,
	xlab, xlim, ylab, ylim, zlab, zlim, asp, layout,
	col, colorscale, colorkey, subset, add)
{
	call <- match.call()
	e <- environment(formula)
	x <- args$rhs[[1]]
	y <- args$rhs[[2]]
	is3d <- length(args$rhs) == 3L
	if ( is3d ) {
		z <- args$rhs[[3]]
	} else {
		z <- NULL
	}
	values <- args$lhs
	n <- nrow(coord(obj))
	if ( any(lengths(values) != n) || length(x) != n || length(y) != n )
		stop("variable lengths differ")
	if ( is3d && length(z) != n )
		stop("variable lengths differ")
	if ( superpose && !is.null(groups) )
		stop("cannot specify 'superpose' and 'groups' in same call")
	more_dims <- length(coord(obj)) > length(args$rhs)
	if ( more_dims ) {
		wh <- which(coordnames(obj) %in% names(args$rhs))
		dpages <- .format.data.labels(coord(obj)[,-wh,drop=FALSE])
		dpages <- factor(dpages, levels=unique(dpages))
		has_dpages <- TRUE
	} else if ( !is3d ) {
		dpages <- run(obj)
		if ( length(runNames(obj)) > 1L ) {
			has_dpages <- TRUE
		} else {
			has_dpages <- FALSE
		}
	} else {
		dpages <- rep_len(factor(1), n)
		has_dpages <- FALSE
	}
	if ( !is.null(groups) ) {
		has_groups <- TRUE		
	} else {
		groups <- rep_len(factor(1), n)
		has_groups <- FALSE
	}
	if ( !is.null(subset) ) {
		x <- x[subset]
		y <- y[subset]
		z <- z[subset]
		values <- lapply(values, "[", subset)
		groups <- groups[subset]
		dpages <- dpages[subset]
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
		facets <- rep_len(factor(1), length(values))
		facets <- as.data.frame(facets)
		has_facets <- FALSE
	}
	facet_levels <- unique(facets)
	facet_levels <- lapply(seq_len(nrow(facet_levels)),
		function(i) facet_levels[i,,drop=FALSE])
	values2 <- unlist(values, use.names=FALSE)
	valrange <- range(values2, na.rm=TRUE)
	if ( gridded(obj) ) {
		rx <- resolution(obj)[names(args$rhs)[1]]
		ry <- resolution(obj)[names(args$rhs)[2]]
		if ( is3d ) {
			rz <- resolution(obj)[names(args$rhs)[3]]
			res <- c(rx, ry, rz)
			dim <- .getDimsFromResolution(list(x=x, y=y, z=z), res)
		} else {
			rz <- min(diff(sort(unique(values2))), na.rm=TRUE)
			res <- c(rx, ry)
			dim <- .getDimsFromResolution(list(x=x, y=y), res)
		}
	} else {
		rx <- min(diff(sort(unique(x))), na.rm=TRUE)
		ry <- min(diff(sort(unique(y))), na.rm=TRUE)
		if ( is3d ) {
			rz <- min(diff(sort(unique(z))), na.rm=TRUE)
		} else {
			rz <- min(diff(sort(unique(values2))), na.rm=TRUE)
		}
		res <- NULL
		dim <- NULL
	}
	if ( missing(xlab) )
		xlab <- names(args$rhs)[1]
	if ( missing(ylab) )
		ylab <- names(args$rhs)[2]
	if ( missing(zlab) ) 
		zlab <- names(args$rhs)[3]
	xrange <- range(x, na.rm=TRUE)
	yrange <- range(y, na.rm=TRUE)
	if ( is3d ) {
		zrange <- range(z, na.rm=TRUE)
	} else {
		zrange <- valrange
	}
	if ( missing(xlim) )
		xlim <- xrange + rx * c(-0.5, 0.5)
	if ( missing(ylim) )
		ylim <- yrange + ry * c(-0.5, 0.5)
	if ( missing(zlim) )
		zlim <- zrange + rz * c(-0.5, 0.5)
	if ( !missing(layout) )
		.setup.layout(layout)
	normalize.image <- normalize.image.method(normalize.image)
	contrast.enhance <- contrast.enhance.method(contrast.enhance)
	smooth.image <- smooth.image.method(smooth.image)
	for ( p in levels(dpages) ) {
		for ( f in facet_levels ) {
			facet_ids <- subrows(facets, f)
			for ( i in facet_ids ) {
				vals <- values[[i]]
				v <- names(values)[i]
				if ( has_groups || superpose || !is.numeric(vals) ) {
					if ( has_groups ) {
						levels <- levels(groups)
					} else if ( superpose ) {
						levels <- na.omit(unique(names(values)))
					} else {
						levels <- na.omit(unique(vals))
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
					if ( is.function(colorscale) ) {
						colors <- colorscale(100)
					} else {
						colors <- colorscale
					}
					has_cats <- FALSE
				}
				if ( !is.numeric(vals) )
					vals <- as.numeric(vals)
				for ( g in levels(groups) ) {
					subscripts <- dpages == p
					gi <- groups[subscripts]
					xi <- x[subscripts]
					yi <- y[subscripts]
					zi <- z[subscripts]
					ti <- vals[subscripts]
					ti[gi != g] <- NA
					if ( has_cats ) {
						if ( has_groups ) {
							cat <- g
						} else if ( superpose ) {
							cat <- v
						} else {
							cat <- NULL
						}
						cols <- setNames(colors, levels)
						cols <- cols[cat]
						if ( !is.null(cat) )
							cols <- alpha.colors(cols, 100)
					} else {
						cols <- colors
					}
					if ( is3d ) {
						ti <- contrast.enhance(normalize.image(ti))
						points3d(xi, yi, zi, ti, ...,
							xlab=xlab, ylab=ylab, zlab=zlab,
							xlim=xlim, ylim=ylim, zlim=zlim,
							col=cols, add=add)
					} else if ( !all(is.na(ti)) ) {
						tproj <- projectToRaster(xi, yi, ti, dim=dim, res=res)
						tproj <- smooth.image(contrast.enhance(normalize.image(tproj)))
						xj <- seq(xrange[1], xrange[2], length.out=dim(tproj)[1])
						yj <- seq(yrange[1], yrange[2], length.out=dim(tproj)[2])
						image(xj, yj, tproj, ...,
							xlab=xlab, ylab=ylab,
							xlim=xlim, ylim=rev(ylim), zlim=zlim,
							col=cols, asp=asp, add=add)
					} else {
						plot(0, 0, type='n', ...,
							xlab=xlab, ylab=ylab,
							xlim=xlim, ylim=rev(ylim),
							asp=asp, add=add)
					}
					add <- TRUE
				}
				last <- i == max(facet_ids)
				if ( !superpose || last ) {
					text <- character()
					if ( length(values) > 1L || has_facets ) {
						if ( has_facets ) {
							text <- c(sapply(f, as.character), text)
						} else if ( !superpose ) {
							text <- c(v, text)
						}
					}
					if ( has_dpages )
						text <- c(p, text)
					.draw.strip.labels(strip, text)
					if ( has_cats ) {
						.draw.key(key, levels, colors)
					} else {
						.draw.colorkey(colorkey,
							round(valrange, 2), colors)
					}
				}
				add <- superpose
			}
			add <- FALSE
		}
		add <- FALSE
	}
	invisible(call)
}

