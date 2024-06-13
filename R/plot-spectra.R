
#### Plot spectra ####
## -------------------

# MSImagingExperiment

setMethod("plot", c(x = "MSImagingExperiment", y = "numeric"),
	function(x, y, ...) plot(x, i = y, ...))

setMethod("plot", c(x = "MSImagingExperiment", y = "character"),
	function(x, y, ...) plot(x, formula = y, ...))

setMethod("plot", c(x = "MSImagingExperiment", y = "formula"),
	function(x, y, ...) plot(x, formula = y, ...))

setMethod("plot", c(x = "MSImagingExperiment", y = "missing"),
	function(x,
		formula = intensity ~ mz,
		i = pixels(x, coord=coord, run=run),
		coord = NULL,
		run = NULL,
		...,
		xlab, ylab,
		isPeaks = isCentroided(x))
{
	if ( "pixel" %in% ...names() ) {
		.Deprecated(old="pixel", new="i")
		i <- list(...)$pixel
	}
	if ( missing(i) && is.null(coord) )
		coord <- coord(x)[1L,,drop=FALSE]
	if ( is.null(run) )
		run <- run(x)[1L]
	if ( !missing(coord) && !is.null(i) && length(i) < length(coord[[1L]]) )
		stop("coord value(s) could not be unambiguously matched")
	if ( missing(xlab) && missing(formula) )
		xlab <- expression(italic(m/z))
	if ( missing(ylab) && missing(formula) )
		ylab <- expression(italic(Intensity))
	if ( !is.null(i) && is.null(names(i)) )
		names(i) <- .make_pixelNames(pixelData(x)[i,,drop=FALSE])
	callNextMethod(x, formula=formula, i=i,
		xlab=xlab, ylab=ylab, isPeaks=isPeaks, ...)
})

# MSImagingArrays

setMethod("plot", c(x = "MSImagingArrays", y = "numeric"),
	function(x, y, ...) plot(x, i = y, ...))

setMethod("plot", c(x = "MSImagingArrays", y = "formula"),
	function(x, y, ...) plot(x, formula = y, ...))

setMethod("plot", c(x = "MSImagingArrays", y = "missing"),
	function(x,
		formula = intensity ~ mz,
		i = pixels(x, coord=coord, run=run),
		coord = NULL,
		run = NULL,
		...,
		xlab, ylab,
		isPeaks = isCentroided(x))
{
	if ( "pixel" %in% ...names() ) {
		.Deprecated(old="pixel", new="i")
		i <- list(...)$pixel
	}
	if ( is.null(coord) )
		coord <- coord(x)[1L,,drop=FALSE]
	if ( is.null(run) )
		run <- run(x)[1L]
	if ( missing(xlab) && missing(formula) )
		xlab <- expression(italic(m/z))
	if ( missing(ylab) && missing(formula) )
		ylab <- expression(italic(Intensity))
	if ( !is.null(i) && is.null(names(i)) )
		names(i) <- .make_pixelNames(pixelData(x)[i,,drop=FALSE])
	callNextMethod(x, formula=formula, i=i,
		xlab=xlab, ylab=ylab, isPeaks=isPeaks, ...)
})

# SpectralImagingExperiment

setMethod("plot", c(x = "SpectralImagingExperiment", y = "numeric"),
	function(x, y, ...) plot(x, i = y, ...))

setMethod("plot", c(x = "SpectralImagingExperiment", y = "character"),
	function(x, y, ...) plot(x, formula = y, ...))

setMethod("plot", c(x = "SpectralImagingExperiment", y = "formula"),
	function(x, y, ...) plot(x, formula = y, ...))

setMethod("plot", c(x = "SpectralImagingExperiment", y = "missing"),
	function(x,
		formula,
		i = 1L,
		groups = NULL,
		superpose = FALSE,
		key = TRUE,
	    ...,
		n = Inf,
		downsampler = "lttb",
		isPeaks = FALSE,
		annPeaks = 0)
{
	if ( missing(formula) ) {
		rhs <- names(featureData(x))[1L]
		lhs <- names(spectraData(x))[1L]
		formula <- as.formula(paste0(lhs, "~", rhs))
	} else if ( is.character(formula) ) {
		rhs <- names(featureData(x))[1L]
		lhs <- paste0(formula, collapse="+")
		formula <- as.formula(paste0(lhs, "~", rhs))
		i <- NULL
	}
	parse <- parse_formula(formula)
	if ( length(parse$rhs) != 1L && length(parse$rhs) != 2L )
		stop("formula must specify exactly 1 or 2 domain dimensions")
	if ( !is.null(i) && is.null(names(i)) ) {
		if ( is.null(pixelNames(x)) ) {
			nms <- paste0("i = ", i)
		} else {
			nms <- pixelNames(x)[i]
		}
		names(i) <- make.unique(nms)
	}
	if ( is.null(i) ) {
		lhs <- eval_exprs(parse$lhs, featureData(x))
		nms <- names(parse$lhs)
	} else {
		lhs <- eval_exprs(parse$lhs, spectraData(x),
			i=NULL, j=i, split_along=2L, group=names(i))
		nms <- names(lhs[[1L]])
	}
	rhs <- eval_exprs(parse$rhs, featureData(x))
	if ( superpose ) {
		by <- NULL
		if ( is.null(groups) )
			groups <- nms
	} else {
		by <- nms
	}
	if ( length(processingData(x)) > 0L )
	{
		snm <- unlist(lapply(parse$lhs, all.vars))
		inm <- unlist(lapply(parse$rhs, all.vars))
		if ( !is.null(groups) )
			warning("ignoring 'groups'")
		if ( isTRUE(superpose) )
			warning("ignoring 'superpose'")
		xi <- process(x[,i], spectra=snm,
			index=inm, BPPARAM=NULL)
		plot_orig <- .plot_feature_data(lhs, rhs,
			by=by, groups="original", key=key,
			n=n, downsampler=downsampler,
			isPeaks=FALSE, ...)
		plot_proc <- plot(xi, formula=formula, i=seq_along(i),
			groups="processed", superpose=FALSE,
			n=n, downsampler=downsampler,
			annPeaks="circle", ...)
		plot <- combine(plot_orig, plot_proc)
	} else
	{
		plot <- .plot_feature_data(lhs, rhs,
			by=by, groups=groups, key=key,
			n=n, downsampler=downsampler,
			isPeaks=isPeaks, annPeaks=annPeaks, ...)
	}
	plot
})

# SpectralImagingArrays

setMethod("plot", c(x = "SpectralImagingArrays", y = "numeric"),
	function(x, y, ...) plot(x, i = y, ...))

setMethod("plot", c(x = "SpectralImagingArrays", y = "formula"),
	function(x, y, ...) plot(x, formula = y, ...))

setMethod("plot", c(x = "SpectralImagingArrays", y = "missing"),
	function(x,
		formula,
		i = 1L,
		groups = NULL,
		superpose = FALSE,
		key = TRUE,
	    ...,
		n = Inf,
		downsampler = "lttb",
		isPeaks = FALSE,
		annPeaks = 0)
{
	if ( missing(formula) ) {
		rhs <- names(spectraData(x))[1L]
		lhs <- names(spectraData(x))[2L]
		formula <- as.formula(paste0(lhs, "~", rhs))
	} else if ( is.character(formula) ) {
		stop("character 'formula' not allowed for ",
			sQuote(class(x)[1L]))
	}
	parse <- parse_formula(formula)
	if ( length(parse$rhs) != 1L && length(parse$rhs) != 2L )
		stop("formula must specify exactly 1 or 2 domain dimensions")
	if ( !is.null(i) && is.null(names(i)) ) {
		if ( is.null(pixelNames(x)) ) {
			nms <- paste0("i = ", i)
		} else {
			nms <- pixelNames(x)[i]
		}
		names(i) <- make.unique(nms)
	}
	rhs <- eval_exprs(parse$rhs, spectraData(x), i=i, group=names(i))
	lhs <- eval_exprs(parse$lhs, spectraData(x), i=i, group=names(i))
	if ( superpose ) {
		by <- NULL
		if ( is.null(groups) )
			groups <- unique(names(i))
	} else {
		by <- unique(names(i))
	}
	if ( length(processingData(x)) > 0L )
	{
		snm <- unlist(lapply(parse$lhs, all.vars))
		inm <- unlist(lapply(parse$rhs, all.vars))
		if ( !is.null(groups) )
			warning("ignoring 'groups'")
		if ( isTRUE(superpose) )
			warning("ignoring 'superpose'")
		xi <- process(x[i], spectra=snm,
			index=inm, BPPARAM=NULL)
		plot_orig <- .plot_feature_data(lhs, rhs,
			by=by, groups="original", key=key,
			n=n, downsampler=downsampler,
			isPeaks=FALSE, ...)
		plot_proc <- plot(xi, formula=formula, i=seq_along(i),
			groups="processed", superpose=FALSE,
			n=n, downsampler=downsampler,
			annPeaks="circle", ...)
		plot <- combine(plot_orig, plot_proc)
	} else
	{
		plot <- .plot_feature_data(lhs, rhs,
			by=by, groups=groups, key=key,
			n=n, downsampler=downsampler,
			isPeaks=isPeaks, annPeaks=annPeaks, ...)
	}
	plot
})

# XDataFrame

setMethod("plot", c(x = "XDataFrame", y = "character"),
	function(x, y, ...) plot(x, formula = y, ...))

setMethod("plot", c(x = "XDataFrame", y = "formula"),
	function(x, y, ...) plot(x, formula = y, ...))

setMethod("plot", c(x = "XDataFrame", y = "missing"),
	function(x,
		formula,
		superpose = FALSE,
		key = TRUE,
	    ...,
		n = Inf,
		downsampler = "lttb",
		isPeaks = FALSE,
		annPeaks = 0)
{
	if ( missing(formula) ) {
		if ( length(x) < 2L )
			stop("data frame must have at least 2 columns")
		if ( length(keys(x)) < 1L )
			stop("need at least 1 key column if formula is missing")
		rhs <- unlist(keys(x))[1L]
		lhs <- setdiff(names(x), rhs)[1L]
		formula <- as.formula(paste0(lhs, "~", rhs))
	} else if ( is.character(formula) ) {
		if ( length(keys(x)) < 1L )
			stop("need at least 1 key column if formula is a string")
		rhs <- unlist(keys(x))[1L]
		lhs <- paste0(formula, collapse="+")
		formula <- as.formula(paste0(lhs, "~", rhs))
	}
	parse <- parse_formula(formula, envir=x, eval=TRUE)
	if ( length(parse$rhs) != 1L && length(parse$rhs) != 2L )
		stop("formula must specify exactly 1 or 2 domain dimensions")
	if ( superpose ) {
		by <- NULL
	} else {
		if ( length(parse$lhs) > 1L ) {
			by <- names(parse$lhs)
		} else {
			by <- NULL
		}
	}
	plot <- .plot_feature_data(parse$lhs, parse$rhs,
		by=by, groups=NULL, key=key,
		n=n, downsampler=downsampler,
		isPeaks=isPeaks, annPeaks=annPeaks, ...)
	plot
})

.plot_feature_data <- function(lhs, rhs,
	by, groups, xlab, ylab, ...)
{
	is1d <- length(rhs) < 2L
	if ( isTRUE(attr(lhs, "recursive")) ) {
		vals <- do.call(c, unname(lhs))
	} else {
		vals <- lhs
	}
	x <- rhs[[1L]]
	if ( is1d ) {
		y <- vals
		z <- NULL
	} else {
		y <- rhs[[2L]]
		z <- vals
	}
	if ( missing(xlab) || is.null(xlab) )
		xlab <- names(rhs)[1L]
	if ( missing(ylab) || is.null(ylab) ) {
		if ( is1d ) {
			if ( length(lhs) > 1L ) {
				ylab <- ""
			} else {
				ylab <- names(lhs)
			}
		} else {
			ylab <- names(rhs)[2L]
		}
	}
	if ( !is.null(groups) && !is.factor(groups) )
		groups <- factor(groups, levels=unique(groups))
	if ( !is.null(by) && !is.factor(by) )
		by <- factor(by, levels=unique(by))
	.last$plot <- plot_signal(x, y, z, by=by, group=groups,
		xlab=xlab, ylab=ylab, ...)
	.last$plot
}

