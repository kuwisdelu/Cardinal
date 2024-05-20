
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
		n = Inf,
		downsampler = "lttb",
		isPeaks = FALSE,
		annPeaks = 0)
{
	if ( missing(formula) ) {
		lhs <- names(spectraData(x))[1L]
		rhs <- names(featureData(x))[1L]
		formula <- as.formula(paste0(lhs, "~", rhs))
	} else if ( is.character(formula) ) {
		lhs <- paste0(formula, collapse="+")
		rhs <- names(featureData(x))[1L]
		formula <- as.formula(paste0(lhs, "~", rhs))
		i <- NULL
	}
	parse <- parse_formula(formula)
	ndim <- length(parse$rhs)
	vars <- all.vars(formula)
	if ( ndim == 1L ) {
		vars_x <- vars[length(vars)]
		vars_y <- setdiff(vars, vars_x)
	} else if ( ndim == 2L ) {
		vars_x <- vars[length(vars) - 1L]
		vars_y <- vars[length(vars)]
		vars_z <- setdiff(vars, c(vars_x, vars_y))
	} else {
		stop("must specify exactly 1 or 2 domain dimensions")
	}
	is1d <- ndim < 2L
	X <- featureData(x)[[vars_x]]
	if ( is1d ) {
		if ( is.null(i) ) {
			Y <- as.data.frame(featureData(x)[vars_y])
		} else {		
			Y <- spectraData(x)[[vars_y]][,i,drop=FALSE]
		}
		Z <- NULL
	} else {
		Y <- featureData(x)[[vars_y]]
		if ( is.null(i) ) {
			Z <- as.data.frame(featureData(x)[vars_z])
		} else {		
			Z <- spectraData(x)[[vars_z]][,i,drop=FALSE]
		}
	}
	if ( !is.list(X) ) {
		if ( is1d ) {
			X <- rep.int(list(X), ncol(Y))
		} else {
			X <- rep.int(list(X), ncol(Z))
		}
	}
	if ( !is.list(Y) ) {
		if ( is1d ) {
			Y <- apply(Y, 2L, identity, simplify=FALSE)
		} else {
			Y <- rep.int(list(Y), ncol(Z))
		}
	}
	if ( !is.list(Z) && !is1d )
		Z <- apply(Z, 2L, identity, simplify=FALSE)
	if ( is1d ) {
		vals <- Y
	} else {
		vals <- Z
	}
	if ( is.null(names(i)) ) {
		if ( is.null(names(vals)) ) {
			if ( is.null(pixelNames(x)) ) {
				names(vals) <- paste0("i = ", i)
			} else {
				names(vals) <- pixelNames(x)[i]
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
	if ( length(processingData(x)) > 0L )
	{
		if ( !is.null(groups) )
			warning("ignoring 'groups'")
		if ( isTRUE(superpose) )
			warning("ignoring 'superpose'")
		if ( is1d ) {
			xi <- process(x[,i], spectra=vars_y,
				index=vars_x, BPPARAM=NULL)
		} else {
			xi <- process(x[,i], spectra=vars_z,
				index=c(vars_x, vars_y), BPPARAM=NULL)
		}
		plot_pre <- .plot_spectra_formula(X, Y, Z, formula,
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
		plot <- .plot_spectra_formula(X, Y, Z, formula,
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
		n = Inf,
		downsampler = "lttb",
		isPeaks = FALSE,
		annPeaks = 0)
{
	if ( missing(formula) ) {
		lhs <- names(spectraData(x))[2L]
		rhs <- names(spectraData(x))[1L]
		formula <- as.formula(paste0(lhs, "~", rhs))
	}
	parse <- parse_formula(formula)
	ndim <- length(parse$rhs)
	vars <- all.vars(formula)
	if ( ndim == 1L ) {
		vars_x <- vars[length(vars)]
		vars_y <- setdiff(vars, vars_x)
	} else if ( ndim == 2L ) {
		vars_x <- vars[length(vars) - 1L]
		vars_y <- vars[length(vars)]
		vars_z <- setdiff(vars, c(vars_x, vars_y))
	} else {
		stop("must specify exactly 1 or 2 domain dimensions")
	}
	is1d <- ndim < 2L
	X <- spectraData(x)[[vars_x]][i]
	Y <- spectraData(x)[[vars_y]][i]
	if ( is1d ) {
		Z <- NULL
	} else {
		Z <- spectraData(x)[[vars_z]][i]
	}
	if ( is1d ) {
		vals <- Y
	} else {
		vals <- Z
	}
	if ( is.null(names(i)) ) {
		if ( is.null(names(vals)) ) {
			if ( is.null(pixelNames(x)) ) {
				names(vals) <- paste0("i = ", i)
			} else {
				names(vals) <- pixelNames(x)[i]
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
	if ( length(processingData(x)) > 0L )
	{
		if ( !is.null(groups) )
			warning("ignoring 'groups'")
		if ( isTRUE(superpose) )
			warning("ignoring 'superpose'")
		if ( is1d ) {
			xi <- process(x[i], spectra=vars_y,
				index=vars_x, BPPARAM=NULL)
		} else {
			xi <- process(x[i], spectra=vars_z,
				index=c(vars_x, vars_y), BPPARAM=NULL)
		}
		plot_pre <- .plot_spectra_formula(X, Y, Z, formula,
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
		plot <- .plot_spectra_formula(X, Y, Z, formula,
			by=by, groups=groups, key=key,
			n=n, downsampler=downsampler,
			isPeaks=isPeaks, annPeaks=annPeaks, ...)
	}
	.lastplot$spectrum <- plot
	plot
})

.plot_spectra_formula <- function(x, y, z,
	formula, by, groups, xlab, ylab, ...)
{
	parse <- parse_formula(formula)
	ndim <- length(parse$rhs)
	vars <- all.vars(formula)
	if ( ndim == 1L ) {
		vars_x <- vars[length(vars)]
		vars_y <- setdiff(vars, vars_x)
	} else if ( ndim == 2L ) {
		vars_x <- vars[length(vars) - 1L]
		vars_y <- vars[length(vars)]
		vars_z <- setdiff(vars, c(vars_x, vars_y))
	} else {
		stop("must specify exactly 1 or 2 domain dimensions")
	}
	is1d <- ndim < 2L
	len <- max(length(x), length(y), length(z))
	if ( is1d ) {
		fm_x <- rep_len(parse$rhs, len)
		fm_y <- rep_len(parse$lhs, len)
	} else {
		fm_x <- rep_len(parse$rhs[1L], len)
		fm_y <- rep_len(parse$rhs[2L], len)
		fm_z <- rep_len(parse$lhs, len)
	}
	names(x) <- rep_len(vars_x, len)
	names(y) <- rep_len(vars_y, len)
	if ( !is1d )
		names(z) <- rep_len(vars_z, len)
	EVAL <- function(expr, i) {
		data <- c(x[i], y[i], z[i])
		eval(expr, envir=data)
	}
	X <- Map(EVAL, fm_x, seq_len(len))
	Y <- Map(EVAL, fm_y, seq_len(len))
	if ( is1d ) {
		Z <- NULL
	} else {
		Z <- do.call(c, Map(EVAL, fm_z, seq_len(len)))
	}
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
	plot_signal(X, Y, Z, by=by, group=groups,
		xlab=xlab, ylab=ylab, ...)
}

