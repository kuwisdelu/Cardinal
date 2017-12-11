
#### Image plotting for SparseImagingExperiment ####
## ------------------------------------------------

setMethod("image", c(x = "SparseImagingExperiment"),
	function(x, formula,
		feature,
		feature.groups,
		groups = NULL,
		superpose = FALSE,
		strip = TRUE,
		key = superpose || !is.null(groups),
		fun = mean,
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
		colorscale = intensity.colors,
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
		lhs.e=as.env(pixelData(x), enclos=e),
		rhs.e=as.list(coord(x)),
		g.e=as.env(featureData(x), enclos=e))
	is3d <- length(args$rhs) == 3L
	if ( length(args$rhs) != 2L && length(args$rhs) != 3L )
		stop("rhs of formula must include exactly 2 or 3 variables")
	if ( missing(feature.groups) ) {
		feature.groups <- NULL
	} else {
		feature.groups <- eval(substitute(feature.groups),
			envir=as.env(featureData(x), enclos=e))
		if ( !is.factor(groups) ) {
			feature.groups <- factor(feature.groups,
				levels=unique(feature.groups))
		} else {
			feature.groups <- droplevels(feature.groups)
		}
		if ( length(feature.groups) < length(feature) ) {
			feature.groups <- rep_len(feature.groups, feature)
		} else if ( length(feature.groups) > length(feature) ) {
			feature.groups <- feature.groups[feature]
		}
	}
	if ( !missing(groups) ) {
		groups <- eval(substitute(groups),
			envir=as.env(pixelData(x), enclos=e))
		if ( !is.factor(groups) ) {
			groups <- factor(groups, levels=unique(groups))
		} else {
			groups <- droplevels(groups)
		}
		if ( length(groups) != ncol(x) )
			groups <- rep_len(groups, ncol(x))
	}
	if ( !missing(subset) ) {
		subset <- eval(substitute(subset),
			envir=as.env(pixelData(x), enclos=e))
		if ( is.logical(subset) )
			subset <- rep_len(subset, ncol(x))
	}
	if ( is.null(args$lhs) ) {
		if ( !is.null(args$g) ) {
			condition <- as.data.frame(args$g)[feature,,drop=FALSE]
		} else {
			condition <- data.frame(row.names=feature)
		}
		if ( !is.null(feature.groups) ) {
			condition$`..feature.groups..` <- feature.groups
		} else {
			condition$`..feature.groups..` <- factor(1)
		}
		condition[] <- lapply(condition, function(ci) {
			if ( !is.factor(ci) ) {
				ci <- factor(ci, levels=unique(ci))
			} else {
				ci <- droplevels(ci)
			}
		})
		args$lhs <- .fastPixelApply2(x, fun, feature, condition)
		names(args$lhs) <- unique(condition)$`..feature.groups..`
		if ( superpose || is.null(feature.groups) )
			condition$`..feature.groups..` <- NULL
		if ( length(condition) > 0L ) {
			facets <- unique(condition)
		} else {
			facets <- NULL
		}
	} else {
		facets <- NULL
	}
	facet.image(args, formula=formula, obj=x,
		facets=facets, groups=groups,
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

.fastPixelApply2 <- function(object, fun, feature, feature.groups) {
	x <- iData(object)[feature,,drop=FALSE]
	groups <- unique(feature.groups)
	glevels <- lapply(seq_len(nrow(groups)),
		function(i) groups[i,,drop=FALSE])
	if ( length(feature) == 1L ) {
		x <- list(drop(x))
	} else if ( length(glevels) == 1L ) {
		x <- list(apply(x, 2L, fun))
	} else {
		x <- lapply(glevels, function(g) {
			i <- subrows(feature.groups, g)
			apply(x[i,,drop=FALSE], 2L, fun)
		})
	}
	x
}
