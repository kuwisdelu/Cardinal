
#### Plot spectra results ####
## ---------------------------

setMethod("plot", c(x = "SpatialResults", y = "ANY"),
	function(x, y, ..., select = NULL, groups = NULL,
		superpose = TRUE, reducedDims = FALSE)
{
	if ( is.character(y) && length(y) == 1L )
		stop("did you mean to specify 'type'?")
	if ( reducedDims ) {
		.plot_reduced_dims(x, y, select=select,
			groups=groups, ...)
	} else {
		.plot_spectra_results(x, y, select=select,
			superpose=superpose, ...)
	}
})

setMethod("plot", c(x = "ResultsList", y = "ANY"),
	function(x, y = 1L, ...) plot(x, i=y, ...))

setMethod("plot", c(x = "ResultsList", y = "missing"),
	function(x, i = 1L, ..., layout = NULL, free = "")
{
	plots <- lapply(x[i], plot, ...)
	if ( !is.null(layout) ) {
		layout <- rep_len(layout, 2L)
		nrow <- layout[1L]
		ncol <- layout[2L]
		as_facets(plots, nrow=nrow, ncol=ncol, free=free)
	} else {
		as_facets(plots, free=free)
	}
})


.plot_spectra_results <- function(x, y, ...,
	select, superpose, xlab, ylab)
{
	if ( is.character(y) && length(y) == 1L )
		y <- x[[y]]
	y <- as.matrix(y)
	ynames <- colnames(y)
	if ( !missing(select) && !is.null(select) )
		y <- y[,select,drop=FALSE]
	if ( missing(ylab) || is.null(ylab) )
		ylab <- "Importance"
	if ( ncol(featureData(x)) > 0L ) {
		ix <- featureData(x)[[1L]]
		if ( missing(xlab) || is.null(xlab) )
			xlab <- names(featureData(x))[1L]
	} else {
		ix <- seq_len(nrow(y))
		if ( missing(xlab) || is.null(xlab) )
			xlab <- "Index"
	}
	if ( missing(superpose) )
		superpose <- TRUE
	if ( superpose ) {
		by <- NULL
		groups <- colnames(y)
	} else {
		by <- colnames(y)
		groups <- by
	}
	plot <- plot_signal(ix, y, by=by, group=groups,
		xlab=xlab, ylab=ylab, isPeaks=TRUE, ...)
	if ( !is.null(ynames) )
		plot <- set_channel(plot, "color", limits=ynames)
	plot
}

.plot_reduced_dims <- function(x, y, ...,
	select, groups, xlab, ylab, engine)
{
	if ( is.character(y) && length(y) == 1L )
		y <- x[[y]]
	if ( missing(select) || is.null(select) )
		select <- c(1L, 2L)
	if ( length(select) != 2L )
		stop("'select' must specify exactly 2 columns")
	if ( is.numeric(select) ) {
		if ( missing(xlab) || is.null(xlab) )
			xlab <- colnames(y)[1L]
		if ( missing(ylab) || is.null(ylab) )
			ylab <- colnames(y)[2L]
	} else {
		if ( missing(xlab) || is.null(xlab) )
			xlab <- select[1L]
		if ( missing(ylab) || is.null(ylab) )
			ylab <- select[2L]
	}
	plot <- vizi(x=y[,select[1L]], y=y[,select[2L]])
	if ( missing(groups) || is.null(groups) ) {
		plot <- add_mark(plot, "points")
	} else {
		plot <- add_mark(plot, "points", color=groups)
		plot <- set_channel(plot, "color", label="\n")
	}
	plot <- set_channel(plot, "x", label=xlab)
	plot <- set_channel(plot, "y", label=ylab)
	if ( !missing(engine) && !is.null(engine) )
		plot <- set_engine(plot, engine)
	if ( ...length() > 0L )
		plot <- set_par(plot, ...)
	plot
}


#### Plot image results ####
## -------------------------

setMethod("image", c(x = "SpatialResults"),
	function(x, y, ..., select = NULL, subset = TRUE,
		superpose = TRUE)
{
	if ( is.character(y) && length(y) == 1L )
		stop("did you mean to specify 'type'?")
	.plot_image_results(x, y, select=select, subset=subset,
		superpose=superpose, ...)
})

setMethod("image", c(x = "ResultsList"),
	function(x, i = 1L, ..., layout = NULL, free = "")
{
	images <- lapply(x[i], image, ...)
	if ( !is.null(layout) ) {
		layout <- rep_len(layout, 2L)
		nrow <- layout[1L]
		ncol <- layout[2L]
		as_facets(images, nrow=nrow, ncol=ncol, free=free)
	} else {
		as_facets(images, free=free)
	}
})

.plot_image_results <- function(x, y, ...,
	select, subset, superpose, xlab, ylab)
{
	if ( is.character(y) && length(y) == 1L )
		y <- x[[y]]
	if ( is.matrix(y) ) {
		ynames <- colnames(y)
	} else {
		ynames <- NULL
	}
	if ( !missing(select) && !is.null(select) )
	{
		if ( is.matrix(y) ) {
			y <- y[,select,drop=FALSE]
		} else {
			y[!y %in% select] <- NA
		}
	}
	if ( !missing(subset) && !isTRUE(all(subset)) )
	{
		if ( is.matrix(y) ) {
			y <- y[subset,,drop=FALSE]
		} else {
			y <- y[subset]
		}
		runs <- droplevels(run(x)[subset])
		coord <- coord(x)[subset,1:2,drop=FALSE]
	} else {
		runs <- run(x)
		coord <- coord(x)[1:2]
	}
	if ( missing(superpose) )
		superpose <- TRUE
	if ( is.matrix(y) ) {
		groups <- colnames(y)
		if ( superpose ) {
			by <- NULL
		} else {
			by <- groups
		}
	} else {
		groups <- NULL
		by <- NULL
	}
	nval <- NCOL(y)
	nrun <- nlevels(runs)
	FUN <- function(irun, v)
	{
		if ( is.null(dim(v)) ) {
			vi <- v[runs %in% irun]
		} else {
			if ( ncol(v) > 1L ) {
				vi <- v[runs %in% irun,,drop=FALSE]
				if ( is.matrix(vi) )
				{
					vnames <- colnames(vi)
					vi <- apply(vi, 2L, identity, simplify=FALSE)
					names(vi) <- vnames
				}
			} else {
				vi <- v[runs %in% irun,,drop=TRUE]
			}
		}
		vi
	}
	ix <- lapply(levels(runs), FUN, v=coord[[1L]])
	iy <- lapply(levels(runs), FUN, v=coord[[2L]])
	vals <- lapply(levels(runs), FUN, v=y)
	if ( nval > 1L ) {
		ix <- rep(ix, each=nval)
		iy <- rep(iy, each=nval)
		vals <- unlist(vals, recursive=FALSE)
	}
	if ( nrun > 1L ) {
		runs <- rep(levels(runs), each=nval)
		if ( is.null(by) ) {
			by <- runs
		} else {
			by <- paste0(runs, "\n", rep.int(by, nrun))
		}
		if ( !is.null(groups) )
			groups <- rep.int(groups, nrun)
	}
	if ( missing(xlab) )
		xlab <- names(coord)[1L]
	if ( missing(ylab) )
		ylab <- names(coord)[2L]
	plot <- plot_image(ix, iy, vals, by=by, group=groups,
		xlab=xlab, ylab=ylab, ...)
	if ( !is.null(ynames) )
		plot <- set_channel(plot, "color", limits=ynames)
	plot
}
