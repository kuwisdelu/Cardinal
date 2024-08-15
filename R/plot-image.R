
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
		.Error("m/z value(s) could not be unambiguously matched")
	if ( missing(xlab) && missing(formula) )
		xlab <- expression(italic(x))
	if ( missing(ylab) && missing(formula) )
		ylab <- expression(italic(y))
	if ( is.null(featureNames(x)) && !is.null(i) && is.null(names(i)) )
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
			.Error("no features within \u00b1", tol,
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
		rhs <- paste0(iQuote(coordNames(x)), collapse="*")
		lhs <- names(spectraData(x))[1L]
		formula <- as.formula(paste0(lhs, "~", rhs))
	} else if ( is.character(formula) ) {
		rhs <- paste0(iQuote(coordNames(x)), collapse="*")
		lhs <- paste0(iQuote(formula), collapse="+")
		formula <- as.formula(paste0(lhs, "~", rhs))
		i <- NULL
	}
	parse <- parse_formula(formula)
	if ( length(parse$rhs) != 2L && length(parse$rhs) != 3L )
		.Error("formula must specify exactly 2 or 3 spatial dimensions")
	if ( !is.null(i) && is.null(names(i)) ) {
		if ( is.null(featureNames(x)) ) {
			nms <- paste0("i = ", i)
		} else {
			nms <- featureNames(x)[i]
		}
		names(i) <- make.unique(nms)
	}
	if ( !is.null(run) ) {
		if ( !is.character(run) && !is.factor(run) )
			run <- runNames(x)[run]
		if ( !all(subset) ) {
			subset <- rep_len(subset, ncol(x))
			subset <- subset & run(x) %in% run
		} else {
			subset <- run(x) %in% run
		}
	}
	if ( all(subset) ) {
		subset <- NULL
		runs <- run(x)
	} else {
		subset <- rep_len(subset, ncol(x))
		runs <- droplevels(run(x)[subset])
	}
	if ( is.null(i) ) {
		lhs <- eval_exprs(parse$lhs, pixelData(x), i=subset)
		nms <- names(parse$lhs)
	} else {
		lhs <- eval_exprs(parse$lhs, spectraData(x),
			i=i, j=subset, split_along=1L, group=names(i))
		nms <- names(lhs[[1L]])
	}
	rhs <- eval_exprs(parse$rhs, pixelData(x), i=subset)
	if ( superpose ) {
		by <- NULL
		if ( is.null(groups) )
			groups <- nms
	} else {
		by <- nms
	}
	plot <- .plot_pixels(lhs, rhs,
		by=by, groups=groups, runs=runs, key=key,
		enhance=enhance, smooth=smooth, scale=scale, ...)
	.last$subset <- subset
	plot
})

# PositionDataFrame

setMethod("image", c(x = "PositionDataFrame"),
	function(x,
		formula,
		run = NULL,
		superpose = FALSE,
		key = TRUE,
	    ...,
		enhance = NULL,
		smooth = NULL,
		scale = NULL,
		subset = TRUE)
{
	if ( missing(formula) || is.numeric(formula) ) {
		rhs <- paste0(iQuote(coordNames(x)), collapse="*")
		lhs <- setdiff(names(x), coordNames(x))[1L]
		formula <- as.formula(paste0(lhs, "~", rhs))
	} else if ( is.character(formula) ) {
		rhs <- paste0(iQuote(coordNames(x)), collapse="*")
		lhs <- paste0(iQuote(formula), collapse="+")
		formula <- as.formula(paste0(lhs, "~", rhs))
	}
	parse <- parse_formula(formula, envir=x, eval=TRUE)
	if ( length(parse$rhs) != 2L && length(parse$rhs) != 3L )
		.Error("formula must specify exactly 2 or 3 spatial dimensions")
	if ( !is.null(run) ) {
		if ( !is.character(run) && !is.factor(run) )
			run <- runNames(x)[run]
		if ( !all(subset) ) {
			subset <- rep_len(subset, ncol(x))
			subset <- subset & run(x) %in% run
		} else {
			subset <- run(x) %in% run
		}
	}
	if ( all(subset) ) {
		subset <- NULL
		runs <- run(x)
	} else {
		subset <- rep_len(subset, ncol(x))
		runs <- droplevels(run(x)[subset])
	}
	if ( !is.null(subset) ) {
		parse$lhs <- lapply(parse$lhs, function(lhs) lhs[subset])
		parse$rhs <- lapply(parse$rhs, function(rhs) rhs[subset])
	}
	if ( superpose ) {
		by <- NULL
	} else {
		if ( length(parse$lhs) > 1L ) {
			by <- names(parse$lhs)
		} else {
			by <- NULL
		}
	}
	plot <- .plot_pixels(parse$lhs, parse$rhs,
		by=by, groups=NULL, runs=runs, key=key,
		enhance=enhance, smooth=smooth, scale=scale, ...)
	.last$subset <- subset
	plot
})

.plot_pixels <- function(lhs, rhs,
	by, groups, runs, xlab, ylab, zlab, ...)
{
	is2d <- length(rhs) < 3L
	if ( isTRUE(attr(lhs, "recursive")) ) {
		vals <- do.call(c, unname(lhs))
	} else {
		vals <- lhs
	}
	x <- rhs[[1L]]
	y <- rhs[[2L]]
	if ( is2d ) {
		z <- NULL
	} else {
		z <- rhs[[3L]]
	}
	if ( missing(xlab) || is.null(xlab) )
		xlab <- names(rhs)[1L]
	if ( missing(ylab) || is.null(ylab) )
		ylab <- names(rhs)[2L]
	if ( missing(zlab) || is.null(zlab) ) {
		if ( is2d ) {
			zlab <- NULL
		} else {
			zlab <- names(rhs)[3L]
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
		runids <- factor(rep.int(levels(runs), length(vals)))
		if ( nlevels(runs) > 1L )
		{
			if ( is.null(by) ) {
				by <- runids
			} else {
				by <- paste0(runids, "\n", by)
			}
			if ( !is.list(x) )
				x <- list(x)
			if ( !is.list(y) )
				y <- list(y)
			if ( !is.list(z) && !is.null(z) )
				z <- list(z)
			if ( !is.list(vals) )
				vals <- list(vals)
			subset_runs <- function(a)
			{
				lapply(levels(runs),
					function(irun) a[runs %in% irun])
			}
			x <- unlist(lapply(x, subset_runs), recursive=FALSE)
			y <- unlist(lapply(y, subset_runs), recursive=FALSE)
			if ( !is.null(z) )
				z <- unlist(lapply(z, subset_runs), recursive=FALSE)
			vals <- unlist(lapply(vals, subset_runs), recursive=FALSE)
		}
	}
	.last$image <- plot_image(x, y, z, vals, by=by, group=groups,
		xlab=xlab, ylab=ylab, zlab=zlab, ...)
	.last$image
}

