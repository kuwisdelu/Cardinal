
#### Plot images ####
## -------------------

# MSImagingExperiment

setMethod("image", c(x = "MSImagingExperiment"),
	function(x,
		formula = intensity ~ x * y,
		i = features(x, mz=mz),
		mz = NULL,
		tolerance = NA,
		units = c("ppm", "mz"),
		...,
		xlab, ylab)
{
	if ( "feature" %in% ...names() ) {
		.Deprecated(old="feature", new="i")
		i <- list(...)$feature
	}
	if ( missing(i) && is.null(mz) )
		mz <- mz(x)[1L]
	if ( !missing(mz) && !is.null(i) && length(i) < length(mz) )
		stop("m/z value(s) could not be unambiguously matched")
	if ( missing(xlab) && missing(formula) )
		xlab <- expression(italic(x))
	if ( missing(ylab) && missing(formula) )
		ylab <- expression(italic(y))
	if ( is.null(names(i)) )
		names(i) <- .make_featureNames(featureData(x)[i,,drop=FALSE], mz(x)[i])
	if ( "plusminus" %in% ...names() ) {
		.Deprecated(old="plusminus", new="tolerance")
		tolerance <- list(...)$plusminus
	}
	if ( is.finite(tolerance) ) {
		if ( is.null(mz) )
			mz <- mz(x)[i]
		if ( missing(units) && !missing(tolerance) )
			units <- get_units_from_names(tolerance, units)
		units <- match.arg(units)
		tol <- switch(units, ppm=1e-6 * tolerance, mz=tolerance)
		ref <- switch(units, ppm="x", mz="abs")
		i <- kdsearch(mz, mz(x), tol=tol, tol.ref=ref)
		if ( any(lengths(i) == 0L) ) {
			nomz <- paste0(mz[which(lengths(i) == 0L)], collapse=", ")
			stop("no features within \u00b1", tol,
				switch(units, ppm=" ppm", mz=""),
				" for m/z = ", nomz)
		}
		nms <- paste0("m/z = ", round(mz, digits=4L))
		nms <- paste0(nms, " \u00b1 ", tolerance, switch(units, ppm=" ppm", mz=""))
		i <- Map(function(j, nm) setNames(j, rep.int(nm, length(j))), i, nms)
		i <- unlist(i)
	}
	callNextMethod(x, formula=formula, i=i,
		xlab=xlab, ylab=ylab, ...)
})

setMethod("image3D", c(x = "MSImagingExperiment"),
	function(x, formula = intensity ~ x * y * z, ...)
{
	image(x, formula=formula, ...)
})

# SpectralImagingExperiment

setMethod("image", c(x = "SpectralImagingExperiment"),
	function(x,
		formula,
		i = 1L,
		run = NULL,
		groups = NULL,
		superpose = FALSE,
		key = TRUE,
	    ...,
		enhance = NULL,
		smooth = NULL,
		scale = NULL,
		subset = TRUE)
{
	if ( missing(formula) || is.numeric(formula) ) {
		if ( !missing(formula) )
			i <- formula
		lhs <- names(spectraData(x))[1L]
		rhs <- paste0(coordNames(x), collapse="*")
		formula <- as.formula(paste0(lhs, "~", rhs))
	} else if ( is.character(formula) ) {
		lhs <- paste0(formula, collapse="+")
		rhs <- paste0(coordNames(x), collapse="*")
		formula <- as.formula(paste0(lhs, "~", rhs))
		i <- NULL
	}
	parse <- parse_formula(formula)
	ndim <- length(parse$rhs)
	vars <- all.vars(formula)
	if ( ndim == 2L ) {
		vars_x <- vars[length(vars) - 1L]
		vars_y <- vars[length(vars)]
		vars_vals <- setdiff(vars, c(vars_x, vars_y))
	} else if ( ndim == 3L ) {
		vars_x <- vars[length(vars) - 2L]
		vars_y <- vars[length(vars) - 1L]
		vars_z <- vars[length(vars)]
		vars_vals <- setdiff(vars, c(vars_x, vars_y, vars_z))
	} else {
		stop("must specify exactly 2 or 3 spatial dimensions")
	}
	is2d <- ndim < 3L
	X <- pixelData(x)[[vars_x]]
	Y <- pixelData(x)[[vars_y]]
	if ( is2d ) {
		Z <- NULL
	} else {
		Z <- pixelData(x)[[vars_z]]
	}
	if ( is.null(i) ) {
		vals <- as.data.frame(pixelData(x)[vars_vals])
	} else {
		vals <- spectraData(x)[[vars_vals]][i,,drop=FALSE]
	}
	if ( !is.null(names(i)) && anyDuplicated(names(i)) ) {
		vals <- rowsum(vals, group=names(i), reorder=FALSE, na.rm=TRUE)
		i <- setNames(rep.int(NA, nrow(vals)), unique(names(i)))
	}
	if ( !is.null(run) )
	{
		if ( !is.character(run) && !is.factor(run) )
			run <- runNames(x)[run]
		if ( !all(subset) ) {
			subset <- rep_len(subset, ncol(x))
			subset <- subset & run(x) %in% run
		} else {
			subset <- run(x) %in% run
		}
	}
	if ( !all(subset) ) {
		subset <- rep_len(subset, ncol(x))
		X <- X[subset]
		Y <- Y[subset]
		Z <- Z[subset]
		if ( is.data.frame(vals) ) {
			vals <- vals[subset,,drop=FALSE]
		} else {
			vals <- vals[,subset,drop=FALSE]
		}
		runs <- droplevels(run(x)[subset])
	} else {
		runs <- run(x)
	}
	if ( is.data.frame(vals) ) {
		X <- rep.int(list(X), ncol(vals))
		Y <- rep.int(list(Y), ncol(vals))
		Z <- rep.int(list(Z), ncol(vals))
		vals <- as.list(vals)
	} else {
		X <- rep.int(list(X), nrow(vals))
		Y <- rep.int(list(Y), nrow(vals))
		Z <- rep.int(list(Z), nrow(vals))
		vals <- apply(vals, 1L, identity, simplify=FALSE)
	}
	if ( is.null(names(i)) ) {
		if ( is.null(names(vals)) ) {
			if ( is.null(featureNames(x)) ) {
				names(vals) <- paste0("i = ", i)
			} else {
				names(vals) <- featureNames(x)[i]
			}
		}
	} else {
		names(vals) <- names(i)
	}
	if ( superpose ) {
		by <- NULL
		if ( is.null(groups) )
			groups <- names(vals)
	} else {
		by <- names(vals)
	}
	plot <- .plot_image_formula(X, Y, Z, vals, formula,
		by=by, groups=groups, runs=runs, key=key,
		enhance=enhance, smooth=smooth, scale=scale, ...)
	.lastplot$subset <- subset
	.lastplot$image <- plot
	plot
})

.plot_image_formula <- function(x, y, z, vals,
	formula, by, groups, runs, xlab, ylab, zlab, ...)
{
	parse <- parse_formula(formula)
	ndim <- length(parse$rhs)
	vars <- all.vars(formula)
	if ( ndim == 2L ) {
		vars_x <- vars[length(vars) - 1L]
		vars_y <- vars[length(vars)]
		vars_vals <- setdiff(vars, c(vars_x, vars_y))
	} else if ( ndim == 3L ) {
		vars_x <- vars[length(vars) - 2L]
		vars_y <- vars[length(vars) - 1L]
		vars_z <- vars[length(vars)]
		vars_vals <- setdiff(vars, c(vars_x, vars_y, vars_z))
	} else {
		stop("must specify exactly 2 or 3 spatial dimensions")
	}
	is2d <- ndim < 3L
	len <- max(length(x), length(y), length(z), length(vals))
	fm_vals <- rep_len(parse$lhs, len)
	fm_x <- rep_len(parse$rhs[1L], len)
	fm_y <- rep_len(parse$rhs[2L], len)
	names(vals) <- rep_len(vars_vals, len)
	names(x) <- rep_len(vars_x, len)
	names(y) <- rep_len(vars_y, len)
	if ( !is2d ) {
		fm_z <- rep_len(parse$rhs[3L], len)
		names(z) <- rep_len(vars_z, len)
	}
	EVAL <- function(expr, i) {
		data <- c(vals[i], x[i], y[i], z[i])
		ans <- eval(expr, envir=data)
		if ( is2d )
			ans <- lapply(levels(runs),
				function(irun) ans[runs %in% irun])
		ans
	}
	VALS <- do.call(c, Map(EVAL, fm_vals, seq_len(len)))
	X <- do.call(c, Map(EVAL, fm_x, seq_len(len)))
	Y <- do.call(c, Map(EVAL, fm_y, seq_len(len)))
	if ( is2d ) {
		Z <- NULL
	} else {
		Z <- do.call(c, Map(EVAL, fm_z, seq_len(len)))
	}
	if ( missing(xlab) || is.null(xlab) )
		xlab <- deparse1(fm_x[[1L]])
	if ( missing(ylab) || is.null(ylab) )
		ylab <- deparse1(fm_y[[1L]])
	if ( missing(zlab) || is.null(zlab) ) {
		if ( is2d ) {
			zlab <- NULL
		} else {
			zlab <- deparse1(fm_z[[1L]])
		}
	}
	if ( !is.null(groups) ) {
		if ( is2d )
			groups <- rep(groups, each=nlevels(runs))
		if ( !is.factor(groups) )
			groups <- factor(groups, levels=unique(groups))
	}
	if ( !is.null(by) ) {
		if ( is2d )
			by <- rep(by, each=nlevels(runs))
		if ( !is.factor(by) )
			by <- factor(by, levels=unique(by))
	}
	if ( is2d )
	{
		runs <- factor(rep.int(levels(runs), len))
		if ( nlevels(runs) > 1L ) {
			if ( is.null(by) ) {
				by <- runs
			} else {
				by <- paste0(runs, "\n", by)
			}
		}
	}
	plot_image(X, Y, Z, VALS, by=by, group=groups,
		xlab=xlab, ylab=ylab, zlab=zlab, ...)
}

