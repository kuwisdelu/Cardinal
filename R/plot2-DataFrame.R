
#### X-Y plotting for XDataFrame ####
## ----------------------------------

setMethod("plot", c(x = "DataFrame", y = "ANY"),
	function(x, y, ...) {
		if ( !missing(y) ) {
			plot(as(x, "XDataFrame"), formula=y, ...)
		} else {
			plot(as(x, "XDataFrame"), ...)
		}
	})

setMethod("plot", c(x = "XDataFrame", y = "formula"),
	function(x, y, ...) plot(x, formula = y, ...))

setMethod("plot", c(x = "XDataFrame", y = "missing"),
	function(x, formula,
		groups = NULL,
		superpose = FALSE,
		strip = TRUE,
		key = superpose || !is.null(groups),
	    ...,
		xlab, xlim,
		ylab, ylim,
		layout = !add,
		col = discrete.colors,
		breaks = "Sturges",
		grid = FALSE,
		jitter = FALSE,
		subset = TRUE,
		add = FALSE)
{
	if ( missing(formula) ) {
		xnm <- setdiff(ls(as.env(x)), names(x))
		if ( length(xnm) > 0L ) {
			xnm <- xnm[1L]
			if ( ncol(x) > 0L ) {
				ynm <- names(x)[1L]
			} else {
				ynm <- xnm
			}
		} else {
			xnm <- names(x)[1L]
			if ( ncol(x) > 1L ) {
				ynm <- names(x)[2L]
			} else {
				ynm <- xnm
			}
		}
		fm <- paste0(ynm, "~", xnm)
		formula <- as.formula(fm, env=parent.frame(1))
	}
	e <- environment(formula)
	args <- .parseFormula2(formula,
		lhs.e=as.env(x, enclos=e),
		rhs.e=as.env(x, enclos=e),
		g.e=as.env(x, enclos=e))
	if ( length(args$rhs) != 1L ) {
		if ( is.null(args$lhs) ) {
			discrete <- sapply(args$rhs, is.discrete)
			if ( !all(discrete) && !(all(!discrete)) )
				.stop("rhs of formula must contain either",
					" all discrete or all continuous variables",
					" in a 1-sided formula")
		} else if ( is.discrete(args$rhs[[1L]]) ) {
			xnm <- names(args$rhs)
			xi <- do.call("interaction", c(args$rhs, list(sep=":")))
			args$rhs <- list(xi)
			names(args$rhs) <- paste0(xnm, collapse=":")
		} else {
			.stop("rhs of formula must include exactly 1 variable",
				" when plotting continuous data")
		}
	}
	if ( !is.null(args$g) ) {
		args$g <- lapply(args$g, as.factor)
		facets <- lapply(args$g, levels)
		facets <- expand.grid(facets)
		if ( length(args$lhs) > 1L )
			.stop("lhs of formula must include exactly 1 variable",
				" when specifying grouping variables with |")
		if ( is.null(args$lhs) && length(args$rhs) > 1L )
			.stop("rhs of a formula must include exactly 1 variable",
				" when specifying grouping variables with |",
				" in a 1-sided formula")
		if ( is.null(args$lhs) ) {
			xnm <- names(args$rhs)
			g <- do.call("interaction", c(args$g, list(sep=":")))
			args$rhs <- lapply(args$rhs, function(x) {
				xg <- lapply(levels(g), function(gi) {
					replace(x, g != gi, NA)
				})
				xg
			})
			args$rhs <- do.call("c", args$rhs)
			names(args$rhs) <- rep(xnm, length(args$rhs))
		} else {
			ynm <- names(args$lhs)
			g <- do.call("interaction", c(args$g, list(sep=":")))
			args$lhs <- lapply(args$lhs, function(y) {
				yg <- lapply(levels(g), function(gi) {
					replace(y, g != gi, NA)
				})
				yg
			})
			args$lhs <- do.call("c", args$lhs)
			names(args$lhs) <- rep(ynm, length(args$lhs))
		}
	} else {
		facets <- NULL
	}
	if ( !missing(groups) ) {
		groups <- .try_eval(substitute(groups), envir=as.env(x, enclos=e))
		if ( !is.factor(groups) ) {
			groups <- factor(groups, levels=unique(groups))
		} else {
			groups <- droplevels(groups)
		}
		if ( length(groups) != nrow(x) )
			groups <- rep_len(groups, nrow(x))
	} else if ( length(groups(x)) ) {
		groups <- groups(x)[[1L]]
	}
	if ( !missing(subset) ) {
		subset <- .try_eval(substitute(subset), envir=as.env(x, enclos=e))
		if ( is.logical(subset) )
			subset <- rep_len(subset, nrow(x))
	}
	if ( is.null(args$lhs) ) {
		facet.count(args, formula=formula, obj=x,
			facets=facets, groups=groups,
			superpose=superpose,
			strip=strip, key=key,
			...,
			xlab=xlab, xlim=xlim,
			ylab=ylab, ylim=ylim,
			layout=layout,
			col=col, grid=grid, breaks=breaks,
			subset=subset, add=add)
	} else if ( is.discrete(args$rhs[[1L]]) ) {
		facet.boxplot(args, formula=formula, obj=x,
			facets=facets, groups=groups,
			superpose=superpose,
			strip=strip, key=key,
			...,
			xlab=xlab, xlim=xlim,
			ylab=ylab, ylim=ylim,
			layout=layout,
			col=col, grid=grid, jitter=jitter,
			subset=subset, add=add)
	} else {
		facet.plot(args, formula=formula, obj=x,
			facets=facets, groups=groups,
			superpose=superpose,
			strip=strip, key=key,
			...,
			xlab=xlab, xlim=xlim,
			ylab=ylab, ylim=ylim,
			layout=layout,
			col=col, grid=grid, jitter=jitter,
			subset=subset, add=add)
	}	
})


# methods for MassDataFrame

setMethod("plot", c(x = "MassDataFrame", y = "formula"),
	function(x, y, ...) plot(x, formula = y, ...))

setMethod("plot", c(x = "MassDataFrame", y = "missing"),
	function(x, ..., type = if (isCentroided(x)) "h" else "l") {
		callNextMethod(x, ..., type=type)
	})


