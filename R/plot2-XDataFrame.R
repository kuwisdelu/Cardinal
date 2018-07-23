
#### X-Y plotting for XDataFrame ####
## ----------------------------------

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
		layout,
		col = discrete.colors,
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
		formula <- as.formula(fm, env=parent.frame(2))
	}
	e <- environment(formula)
	args <- .parseFormula2(formula,
		lhs.e=as.env(x, enclos=e),
		rhs.e=as.env(x, enclos=e))
	if ( length(args$rhs) != 1L )
		.stop("rhs of formula must include exactly 1 variable")
	if ( !is.null(args$g) )
		.stop("conditioning variables via | not allowed")
	if ( !missing(groups) ) {
		groups <- .try_eval(substitute(groups), envir=as.env(x, enclos=e))
		if ( !is.factor(groups) ) {
			groups <- factor(groups, levels=unique(groups))
		} else {
			groups <- droplevels(groups)
		}
		if ( length(groups) != nrow(x) )
			groups <- rep_len(groups, nrow(x))
	}
	if ( !missing(subset) ) {
		subset <- .try_eval(substitute(subset), envir=as.env(x, enclos=e))
		if ( is.logical(subset) )
			subset <- rep_len(subset, nrow(x))
	}
	facet.plot(args, formula=formula, obj=x,
		facets=NULL, groups=groups,
		superpose=superpose,
		strip=strip, key=key,
		...,
		xlab=xlab, xlim=xlim,
		ylab=ylab, ylim=ylim,
		layout=layout, col=col,
		subset=subset, add=add)
})

