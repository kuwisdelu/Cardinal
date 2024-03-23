
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
		rhs <- names(featureData(x))[1L]
		formula <- as.formula(paste0(formula, "~", rhs))
		i <- NULL
	}
	vars <- all.vars(formula)
	if ( length(formula) != 3L || length(vars) != 2L )
		stop("formula must specify exactly 2 variables")
	if ( is.null(i) ) {
		X <- as.list(featureData(x)[vars[2L]])
		Y <- as.list(featureData(x)[vars[1L]])
	} else {
		X <- featureData(x)[[vars[2L]]]
		Y <- spectraData(x)[[vars[1L]]][,i,drop=FALSE]
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
		xi <- process(x[,i], spectra=vars[1L], index=vars[2L], BPPARAM=NULL)
		plot1 <- .plot_spectra_formula(X, Y, formula,
			by=by, groups="original", key=key,
			n=n, downsampler=downsampler,
			isPeaks=FALSE, ...)
		plot2 <- plot(xi, formula=formula, i=seq_along(i),
			groups="processed", superpose=FALSE,
			n=n, downsampler=downsampler,
			annPeaks="circle", ...)
		plot <- combine(plot1, plot2)
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
	if ( length(formula) != 3L || length(vars) != 2L )
		stop("formula must specify exactly 2 variables")
	X <- spectraData(x)[[vars[2L]]][i]
	Y <- spectraData(x)[[vars[1L]]][i]
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
		xi <- process(x[i], spectra=vars[1L], index=vars[2L], BPPARAM=NULL)
		plot1 <- .plot_spectra_formula(X, Y, formula,
			by=by, groups="original", key=key,
			n=n, downsampler=downsampler,
			isPeaks=FALSE, ...)
		plot2 <- plot(xi, formula=formula, i=seq_along(i),
			groups="processed", superpose=FALSE,
			n=n, downsampler=downsampler,
			annPeaks="circle", ...)
		plot <- combine(plot1, plot2)
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
	fm <- list(x=formula[[3L]], y=formula[[2L]])
	FUN <- function(yi, xi, e) {
		df <- setNames(list(yi, xi), vars)
		eval(e, envir=df)
	}
	X <- Map(FUN, y, x, rep.int(list(fm$x), length(x)))
	Y <- Map(FUN, y, x, rep.int(list(fm$y), length(y)))
	if ( missing(xlab) )
		xlab <- as.character(fm$x)
	if ( missing(ylab) )
		ylab <- as.character(fm$y)
	if ( !is.null(groups) && !is.factor(groups) )
		groups <- factor(groups, levels=unique(groups))
	if ( !is.null(by) && !is.factor(by) )
		by <- factor(by, levels=unique(by))
	plot_signal(X, Y, by=by, group=groups,
		xlab=xlab, ylab=ylab, ...)
}

