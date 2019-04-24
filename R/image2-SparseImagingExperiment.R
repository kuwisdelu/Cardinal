
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
		layout = !add,
		col = discrete.colors,
		colorscale = viridis,
		colorkey = !key,
		alpha.power = 1,
		subset = TRUE,
		add = FALSE)
{
	.checkForIncompleteProcessing(x)
	if ( missing(formula) ) {
		valnm <- names(imageData(x))[1L]
		fm <- paste0(valnm, "~", paste0(coordnames(x)[c(1,2)], collapse="*"))
		formula <- as.formula(fm, env=parent.frame(2))
	}
	e <- environment(formula)
	elhs <- as.env(pixelData(x), enclos=e)
	if ( !missing(feature) && !is.null(names(imageData(x))) ) {
		xi <- as.list(imageData(x)[feature,,drop=FALSE])
		multiassign(names(xi), xi, envir=elhs)
	}
	args <- .parseFormula2(formula,
		lhs.e=elhs, rhs.e=as.list(coord(x)),
		g.e=as.env(featureData(x), enclos=e))
	is3d <- length(args$rhs) == 3L
	if ( length(args$rhs) != 2L && length(args$rhs) != 3L )
		.stop("rhs of formula must include exactly 2 or 3 variables")
	if ( missing(feature.groups) ) {
		feature.groups <- NULL
	} else if ( !is.null(feature.groups) ) {
		feature.groups <- .try_eval(substitute(feature.groups),
			envir=as.env(featureData(x), enclos=e))
		if ( !is.factor(feature.groups) ) {
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
		groups <- .try_eval(substitute(groups),
			envir=as.env(pixelData(x), enclos=e))
		if ( !is.factor(groups) ) {
			groups <- factor(groups, levels=unique(groups))
		} else {
			groups <- droplevels(groups)
		}
		if ( length(groups) %% ncol(x) != 0L )
			groups <- rep_len(groups, ncol(x))
	}
	if ( !missing(subset) ) {
		subset <- .try_eval(substitute(subset),
			envir=as.env(pixelData(x), enclos=e))
		if ( !is.logical(subset) ) {
			subset <- replace(logical(ncol(x)), subset, TRUE)
		} else if ( length(subset) %% ncol(x) != 0L ) {
			subset <- rep_len(subset, ncol(x))
		}
	}
	if ( !missing(feature) ) {
		if ( !is.null(args$g) ) {
			condition <- as.data.frame(args$g)[feature,,drop=FALSE]
		} else {
			condition <- data.frame(row.names=make.names(feature, unique=TRUE))
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
		if ( is.null(args$lhs) ) {
			xi <- as.matrix(iData(x)[feature,,drop=FALSE])
			args$lhs <- .fastPixelApply2(xi, fun, condition)
			if ( is.null(feature.groups) && !is.null(names(imageData(x))) ) {
				names(args$lhs) <- rep_len(names(imageData(x))[1], length(args$lhs))
			} else {
				names(args$lhs) <- unique(condition)$`..feature.groups..`
			}
			val.groups <- NULL
		} else {
			val.groups <- factor(names(args$lhs))
			args$lhs <- lapply(args$lhs, function(xi) {
				if ( isTRUE(nrow(xi) == nrow(condition)) ) {
					.fastPixelApply2(xi, fun, condition)
				} else {
					list(as.vector(xi))
				}
			})
			condition <- unique(condition)
			val.groups <- rep.int(val.groups, lengths(args$lhs))
			if ( sum(lengths(args$lhs)) > nrow(condition) ) {
				condition <- lapply(lengths(args$lhs), function(l) {
					if ( l != nrow(condition) ) {
						condition[nrow(condition) + 1L,] # returns NA's
					} else {
						condition
					}
				})
				condition <- do.call("rbind", condition)
				condition$`..val.groups..` <- val.groups
			}
			args$lhs <- unlist(args$lhs, recursive=FALSE)
			names(args$lhs) <- val.groups
		}
		condition <- unique(condition)
		if ( superpose ) {
			if ( is.null(feature.groups) ) {
				if ( length(args$g) == 1L ) {
					names(args$lhs) <- as.character(condition[[names(args$g)]])
					condition[[names(args$g)]] <- NULL
				} else if ( length(args$g) > 1L ) {
					.stop("can't superpose multiple conditioning variables")
				} else {
					condition$`..val.groups..` <- NULL
				}
			} else {
				if ( nlevels(val.groups) > 1L ) {
					names(args$lhs) <- as.character(condition$`..val.groups..`)
					condition$`..val.groups..` <- NULL
				} else {
					names(args$lhs) <- as.character(condition$`..feature.groups..`)
					condition$`..feature.groups..` <- NULL
				}
			}
		}
		if ( is.null(feature.groups) )
			condition$`..feature.groups..` <- NULL
		if ( length(condition) > 0L ) {
			facets <- condition
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
		colorkey=colorkey, alpha.power=alpha.power,
		subset=subset, add=add)
})

setMethod("image3D", c(x = "SparseImagingExperiment"),
	function(x, formula, ..., alpha.power=2) {
		if ( missing(formula) ) {
			valnm <- names(imageData(x))[1L]
			if ( length(coord(x)) < 3L )
				.stop("found only ", length(coord(x)), " spatial dimensions")
			fm <- paste0(valnm, "~", paste0(coordnames(x)[1:3], collapse="*"))
			formula <- as.formula(fm, env=parent.frame(2))
		}
		image(x, formula=formula, ..., alpha.power=2)
	})

.fastPixelApply2 <- function(x, fun, groups) {
	all.groups <- groups
	groups <- unique(groups)
	glevels <- lapply(seq_len(nrow(groups)),
		function(i) groups[i,,drop=FALSE])
	if ( nrow(x) == 1L ) {
		x <- list(drop(x))
	} else if ( length(glevels) == 1L ) {
		x <- list(apply(x, 2L, fun))
	} else {
		x <- lapply(glevels, function(g) {
			i <- subset_rows(all.groups, g)
			apply(x[i,,drop=FALSE], 2L, fun)
		})
	}
	x
}
