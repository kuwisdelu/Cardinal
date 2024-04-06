
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
	if ( is.null(mz) )
		mz <- mz(x)[1L]
	if ( any(mz < min(mz(x))) || any(mz > max(mz(x))) )
		warning("m/z value(s) out of range")
	if ( missing(xlab) && missing(formula) )
		xlab <- expression(italic(x))
	if ( missing(ylab) && missing(formula) )
		ylab <- expression(italic(y))
	if ( is.null(names(i)) )
		names(i) <- .make_featureNames(featureData(x)[i,,drop=FALSE])
	if ( "plusminus" %in% ...names() ) {
		.Deprecated(old="plusminus", new="tolerance")
		tolerance <- plusminus
	}
	if ( is.finite(tolerance) ) {
		if ( is.null(mz) )
			mz <- mz(x)[i]
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
		rhs <- paste0(coordNames(x)[1:2], collapse="*")
		formula <- as.formula(paste0(lhs, "~", rhs))
	} else if ( is.character(formula) ) {
		rhs <- paste0(coordNames(x)[1:2], collapse="*")
		formula <- as.formula(paste0(formula, "~", rhs))
		i <- NULL
	}
	vars <- all.vars(formula)
	if ( length(formula) != 3L || length(vars) != 3L )
		stop("formula must specify exactly 3 variables")
	X <- pixelData(x)[[vars[2L]]]
	Y <- pixelData(x)[[vars[3L]]]
	if ( is.null(i) ) {
		vals <- as.data.frame(pixelData(x)[vars[1L]])
	} else {
		vals <- spectraData(x)[[vars[1L]]][i,,drop=FALSE]
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
		vals <- vals[,subset,drop=FALSE]
		runs <- droplevels(run(x)[subset])
	} else {
		runs <- run(x)
	}
	if ( is.data.frame(vals) ) {
		X <- list(X)
		Y <- list(Y)
		vals <- as.list(vals)
	} else {
		X <- rep.int(list(X), nrow(vals))
		Y <- rep.int(list(Y), nrow(vals))
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
	plot <- .plot_image_formula(X, Y, vals, formula,
		by=by, groups=groups, runs=runs, key=key,
		enhance=enhance, smooth=smooth, scale=scale, ...)
	.lastplot$subset <- subset
	.lastplot$image <- plot
	plot
})

.plot_image_formula <- function(x, y, vals,
	formula, by, groups, runs, xlab, ylab, ...)
{
	vars <- all.vars(formula)
	fm <- list(vals=formula[[2L]],
		x=formula[[3L]][[2L]],
		y=formula[[3L]][[3L]])
	n <- length(vals)
	runs <- factor(rep.int(levels(runs), n))
	nms <- names(vals)
	FUN <- function(zi, xi, yi, e) {
		df <- setNames(list(zi, xi, yi), vars)
		ans <- eval(e, envir=df)
		lapply(levels(runs), function(ri) ans[runs %in% ri])
	}
	X <- do.call(c, Map(FUN, vals, x, y, rep.int(list(fm$x), n)))
	Y <- do.call(c, Map(FUN, vals, x, y, rep.int(list(fm$y), n)))
	vals <- do.call(c, Map(FUN, vals, x, y, rep.int(list(fm$vals), n)))
	names(vals) <- rep(nms, each=nlevels(runs))
	if ( missing(xlab) )
		xlab <- as.character(fm$x)
	if ( missing(ylab) )
		ylab <- as.character(fm$y)
	if ( !is.null(groups) ) {
		groups <- rep(groups, each=nlevels(runs))
		if ( !is.factor(groups) )
			groups <- factor(groups, levels=unique(groups))
	}
	if ( !is.null(by) ) {
		by <- rep(by, each=nlevels(runs))
		if ( !is.factor(by) )
			by <- factor(by, levels=unique(by))
	}
	if ( nlevels(runs) > 1L ) {
		if ( is.null(by) ) {
			by <- runs
		} else {
			by <- paste0(runs, "\n", by)
		}
	}
	plot_image(X, Y, vals, by=by, group=groups,
		xlab=xlab, ylab=ylab, ...)
}


