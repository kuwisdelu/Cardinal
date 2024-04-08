
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
	if ( missing(xlab) && missing(formula) )
		xlab <- expression(italic(m/z))
	if ( missing(ylab) && missing(formula) )
		ylab <- expression(italic(Intensity))
	if ( is.null(names(i)) )
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
	if ( is.null(names(i)) )
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
		n = 2000L,
		downsampler = "lttb",
		isPeaks = FALSE,
		annPeaks = 0)
{
	if ( missing(formula) ) {
		lhs <- names(spectraData(x))[1L]
		rhs <- names(featureData(x))[1L]
		formula <- as.formula(paste0(lhs, "~", rhs))
	} else if ( is.character(formula) ) {
		formula <- paste0(formula, collapse="+")
		rhs <- names(featureData(x))[1L]
		formula <- as.formula(paste0(formula, "~", rhs))
		i <- NULL
	}
	vars <- all.vars(formula)
	vars_x <- vars[length(vars)]
	vars_y <- vars[-length(vars)]
	X <- featureData(x)[[vars_x]]
	if ( is.null(i) ) {
		Y <- as.data.frame(featureData(x)[vars_y])
	} else {		
		Y <- spectraData(x)[[vars_y]][,i,drop=FALSE]
	}
	if ( !is.list(X) )
		X <- rep.int(list(X), ncol(Y))
	if ( !is.list(Y) )
		Y <- apply(Y, 2L, identity, simplify=FALSE)
	if ( is.null(names(i)) ) {
		if ( is.null(names(Y)) ) {
			if ( is.null(pixelNames(x)) ) {
				names(Y) <- paste0("i = ", i)
			} else {
				names(Y) <- pixelNames(x)[i]
			}
		}
	} else {
		names(Y) <- names(i)
	}
	if ( superpose ) {
		by <- NULL
		if ( is.null(groups) )
			groups <- names(Y)
	} else {
		by <- names(Y)
	}
	if ( length(processingData(x)) > 0L )
	{
		if ( !is.null(groups) )
			warning("ignoring 'groups'")
		if ( isTRUE(superpose) )
			warning("ignoring 'superpose'")
		xi <- process(x[,i], spectra=vars_y, index=vars_x, BPPARAM=NULL)
		plot_pre <- .plot_spectra_formula(X, Y, formula,
			by=by, groups="original", key=key,
			n=n, downsampler=downsampler,
			isPeaks=FALSE, ...)
		plot_post <- plot(xi, formula=formula, i=seq_along(i),
			groups="processed", superpose=FALSE,
			n=n, downsampler=downsampler,
			annPeaks="circle", ...)
		plot <- combine(plot_pre, plot_post)
	} else
	{
		plot <- .plot_spectra_formula(X, Y, formula,
			by=by, groups=groups, key=key,
			n=n, downsampler=downsampler,
			isPeaks=isPeaks, annPeaks=annPeaks, ...)
	}
	.lastplot$spectrum <- plot
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
		n = 2000L,
		downsampler = "lttb",
		isPeaks = FALSE,
		annPeaks = 0)
{
	if ( missing(formula) ) {
		lhs <- names(spectraData(x))[2L]
		rhs <- names(spectraData(x))[1L]
		formula <- as.formula(paste0(lhs, "~", rhs))
	}
	vars <- all.vars(formula)
	vars_x <- vars[length(vars)]
	vars_y <- vars[-length(vars)]
	X <- spectraData(x)[[vars_x]][i]
	Y <- spectraData(x)[[vars_y]][i]
	if ( is.null(names(i)) ) {
		if ( is.null(names(Y)) ) {
			if ( is.null(pixelNames(x)) ) {
				names(Y) <- paste0("i = ", i)
			} else {
				names(Y) <- pixelNames(x)[i]
			}
		}
	} else {
		names(Y) <- names(i)
	}
	if ( superpose ) {
		by <- NULL
		if ( is.null(groups) )
			groups <- names(Y)
	} else {
		by <- names(Y)
	}
	if ( length(processingData(x)) > 0L )
	{
		if ( !is.null(groups) )
			warning("ignoring 'groups'")
		if ( isTRUE(superpose) )
			warning("ignoring 'superpose'")
		xi <- process(x[i], spectra=vars_y, index=vars_x, BPPARAM=NULL)
		plot_pre <- .plot_spectra_formula(X, Y, formula,
			by=by, groups="original", key=key,
			n=n, downsampler=downsampler,
			isPeaks=FALSE, ...)
		plot_post <- plot(xi, formula=formula, i=seq_along(i),
			groups="processed", superpose=FALSE,
			n=n, downsampler=downsampler,
			annPeaks="circle", ...)
		plot <- combine(plot_pre, plot_post)
	} else
	{
		plot <- .plot_spectra_formula(X, Y, formula,
			by=by, groups=groups, key=key,
			n=n, downsampler=downsampler,
			isPeaks=isPeaks, annPeaks=annPeaks, ...)
	}
	.lastplot$spectrum <- plot
	plot
})

.plot_spectra_formula <- function(x, y,
	formula, by, groups, xlab, ylab, ...)
{
	vars <- all.vars(formula)
	vars_x <- vars[length(vars)]
	vars_y <- vars[-length(vars)]
	fm_x <- as.list(formula[[3L]])
	fm_y <- as.list(formula[[2L]])
	if ( length(fm_x) > 1L )
		fm_x <- fm_x[-1L]
	if ( length(fm_y) > 1L )
		fm_y <- fm_y[-1L]
	len <- max(length(x), length(y))
	vars_x <- rep_len(vars_x, len)
	vars_y <- rep_len(vars_y, len)
	fm_x <- rep_len(fm_x, len)
	fm_y <- rep_len(fm_y, len)
	names(fm_x) <- vars_x
	names(fm_y) <- vars_y
	names(x) <- vars_x
	names(y) <- vars_y
	EVAL <- function(expr, i) {
		data <- c(x[i], y[i])
		eval(expr, envir=data)
	}
	X <- Map(EVAL, fm_x, seq_len(len))
	Y <- Map(EVAL, fm_y, seq_len(len))
	if ( missing(xlab) || is.null(xlab) )
		xlab <- deparse1(fm_x[[1L]])
	if ( missing(ylab) || is.null(ylab) ) {
		if ( length(fm_y) > 1L ) {
			ylab <- ""
		} else {
			ylab <- deparse1(fm_y[[1L]])
		}
	}
	if ( !is.null(groups) && !is.factor(groups) )
		groups <- factor(groups, levels=unique(groups))
	if ( !is.null(by) && !is.factor(by) )
		by <- factor(by, levels=unique(by))
	plot_signal(X, Y, by=by, group=groups,
		xlab=xlab, ylab=ylab, ...)
}

