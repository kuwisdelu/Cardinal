
#### X-Y plotting for SparseImagingExperiment ####
## ------------------------------------------------

setMethod("plot", c(x = "SparseImagingExperiment", y = "formula"),
	function(x, y, ...) plot(x, formula = y, ...))

setMethod("plot", c(x = "SparseImagingExperiment"),
	function(x, formula,
		pixel,
		pixel.groups,
		groups = NULL,
		superpose = FALSE,
		strip = TRUE,
		key = superpose || !is.null(groups),
		fun = mean,
	    ...,
		xlab, xlim,
		ylab, ylim,
		layout,
		col = discrete.colors,
		subset = TRUE,
		add = FALSE)
{
	if ( missing(formula) ) {
		if ( ncol(featureData(x)) > 0L ) {
			var <- names(featureData(x))[1L]
		} else {
			var <- ls(as.env(featureData(x)))[1L]
		}
		fm <- paste0("~", paste0(var, collapse="*"))
		formula <- as.formula(fm, env=parent.frame(2))
	}
	e <- environment(formula)
	args <- .parseFormula2(formula,
		lhs.e=as.env(featureData(x), enclos=e),
		rhs.e=as.env(featureData(x), enclos=e),
		g.e=as.env(pixelData(x), enclos=e))
	if ( length(args$rhs) != 1L )
		stop("rhs of formula must include exactly 1 variables")
	if ( missing(pixel.groups) ) {
		pixel.groups <- NULL
	} else {
		pixel.groups <- eval(substitute(pixel.groups),
			envir=as.env(pixelData(x), enclos=e))
		if ( !is.factor(pixel.groups) ) {
			pixel.groups <- factor(pixel.groups,
				levels=unique(pixel.groups))
		} else {
			pixel.groups <- droplevels(pixel.groups)
		}
		if ( length(pixel.groups) < length(pixel) ) {
			pixel.groups <- rep_len(pixel.groups, length(pixel))
		} else if ( length(pixel.groups) > length(pixel) ) {
			pixel.groups <- pixel.groups[pixel]
		}
	}
	if ( !missing(groups) ) {
		groups <- eval(substitute(groups),
			envir=as.env(featureData(x), enclos=e))
		if ( !is.factor(groups) ) {
			groups <- factor(groups, levels=unique(groups))
		} else {
			groups <- droplevels(groups)
		}
		if ( length(groups) != nrow(x) )
			groups <- rep_len(groups, nrow(x))
	}
	if ( !missing(subset) ) {
		subset <- eval(substitute(subset),
			envir=as.env(featureData(x), enclos=e))
		if ( is.logical(subset) )
			subset <- rep_len(subset, nrow(x))
	}
	if ( is.null(args$lhs) ) {
		if ( !is.null(args$g) ) {
			condition <- as.data.frame(args$g)[pixel,,drop=FALSE]
		} else {
			condition <- data.frame(row.names=pixel)
		}
		if ( !is.null(pixel.groups) ) {
			condition$`..pixel.groups..` <- pixel.groups
		} else {
			condition$`..pixel.groups..` <- factor(1)
		}
		condition[] <- lapply(condition, function(ci) {
			if ( !is.factor(ci) ) {
				ci <- factor(ci, levels=unique(ci))
			} else {
				ci <- droplevels(ci)
			}
		})
		args$lhs <- .fastFeatureApply2(x, fun, pixel, condition)
		names(args$lhs) <- unique(condition)$`..pixel.groups..`
		if ( superpose || is.null(pixel.groups) )
			condition$`..pixel.groups..` <- NULL
		if ( length(condition) > 0L ) {
			facets <- unique(condition)
		} else {
			facets <- NULL
		}
	} else {
		facets <- NULL
	}
	facet.plot(args, formula=formula, obj=x,
		facets=facets, groups=groups,
		superpose=superpose,
		strip=strip, key=key,
		...,
		xlab=xlab, xlim=xlim,
		ylab=ylab, ylim=ylim,
		layout=layout, col=col,
		subset=subset, add=add)
})

.fastFeatureApply2 <- function(object, fun, pixel, pixel.groups) {
	x <- iData(object)[,pixel,drop=FALSE]
	groups <- unique(pixel.groups)
	glevels <- lapply(seq_len(nrow(groups)),
		function(i) groups[i,,drop=FALSE])
	if ( length(pixel) == 1L ) {
		x <- list(drop(x))
	} else if ( length(glevels) == 1L ) {
		x <- list(apply(x, 1L, fun))
	} else {
		x <- lapply(glevels, function(g) {
			i <- subrows(pixel.groups, g)
			apply(x[,i,drop=FALSE], 1L, fun)
		})
	}
	x
}
