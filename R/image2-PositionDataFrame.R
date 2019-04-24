
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
		colorscale = viridis,
		colorkey = !key,
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
		.stop("rhs of formula must include exactly 2 or 3 variables")
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

