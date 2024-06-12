
#### SpatialResults ####
## ---------------------

SpatialResults <- function(model, data,
	featureData = if (!missing(data)) fData(data) else NULL,
	pixelData = if (!missing(data)) pData(data) else NULL)
{
	new("SpatialResults", model=model,
		featureData=featureData, pixelData=pixelData)
}

setMethod("show", "SpatialResults",
	function(object) {
		# length
		cat(class(object), "on", nrow(featureData(object)), "variables",
			"and", nrow(pixelData(object)), "observations\n")
		# names()
		cat(sprintf("names(%d): %s\n", length(names(object)),
			.paste_head_tail(names(object))))
		# coord()
		if ( length(object) > 0L )
		{
			lims <- vapply(coord(object), range, numeric(2L))
			lims <- paste0(coordNames(object), " = ", lims[1L,], "...", lims[2L,])
			cat(sprintf("coord(%d): %s\n", length(coordNames(object)),
				.paste_head_tail(lims)))
		}
		# runNames()
		cat(sprintf("runNames(%d): %s\n", length(runNames(object)),
			.paste_head_tail(runNames(object))))
		# modelData()
		cat("modelData(): ")
		print(object@model)
	})

## Basic getters and setters

setMethod("length", "SpatialResults", function(x) length(x@model))

setMethod("names", "SpatialResults",
	function(x) names(x@model))

setMethod("[[", "SpatialResults",
	function(x, i, j, ...) x@model[[i, ...]])

setMethod("$", "SpatialResults",
	function(x, name) x@model[[name, exact=FALSE]])

# allow tab completion in console
.DollarNames.SpatialResults <- function(x, pattern = "") {
	grep(pattern, names(x), value=TRUE)
}

## Slot getters and setters

# modelData

setMethod("modelData", "SpatialResults",
	function(object, ...) object@model)
setReplaceMethod("modelData", "SpatialResults",
	function(object, ..., value) {
		object@model <- value
		if ( validObject(object) )
			object
	})

# featureData

setMethod("featureData", "SpatialResults",
	function(object) object@featureData)
setReplaceMethod("featureData", "SpatialResults",
	function(object, value) {
		object@featureData <- value
		if ( validObject(object) )
			object
	})

setMethod("fData", "SpatialResults",
	function(object) featureData(object))
setReplaceMethod("fData", "SpatialResults",
	function(object, value) {
		featureData(object) <- value
		object
	})

setMethod("featureNames", "SpatialResults",
	function(object) rownames(featureData(object)))
setReplaceMethod("featureNames", "SpatialResults",
	function(object, value) {
		rownames(featureData(object)) <- value
			object
	})

# pixelData

setMethod("pixelData", "SpatialResults",
	function(object) object@pixelData)
setReplaceMethod("pixelData", "SpatialResults",
	function(object, value) {
		object@pixelData <- value
		if ( validObject(object) )
			object
	})

setMethod("pData", "SpatialResults",
	function(object) pixelData(object))
setReplaceMethod("pData", "SpatialResults",
	function(object, value) {
		pixelData(object) <- value
		object
	})

setMethod("pixelNames", "SpatialResults",
	function(object) rownames(pixelData(object)))
setReplaceMethod("pixelNames", "SpatialResults",
	function(object, value) {
		rownames(pixelData(object)) <- value
		object
	})

# Coord/Run access

setMethod("coord", "SpatialResults",
	function(object, ...) coord(pixelData(object)))
setReplaceMethod("coord", "SpatialResults",
	function(object, ..., value) {
		coord(pixelData(object)) <- value
		object
	})

setMethod("coordNames", "SpatialResults",
	function(object) coordNames(pixelData(object)))
setReplaceMethod("coordNames", "SpatialResults",
	function(object, value) {
		coordNames(pixelData(object)) <- value
		object
	})

setMethod("run", "SpatialResults",
	function(object, ...) run(pixelData(object)))
setReplaceMethod("run", "SpatialResults",
	function(object, ..., value) {
		run(pixelData(object)) <- value
		object
	})

setMethod("runNames", "SpatialResults",
	function(object) runNames(pixelData(object)))
setReplaceMethod("runNames", "SpatialResults",
	function(object, value) {
		runNames(pixelData(object)) <- value
		object
	})

setMethod("nrun", "SpatialResults",
	function(x) nrun(pixelData(x)))

## Plotting

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

setMethod("image", c(x = "SpatialResults"),
	function(x, y, ..., select = NULL, subset = TRUE,
		superpose = TRUE)
{
	if ( is.character(y) && length(y) == 1L )
		stop("did you mean to specify 'type'?")
	.plot_image_results(x, y, select=select, subset=subset,
		superpose=superpose, ...)
})

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


#### ResultsList ####
## ------------------

.valid_ResultsList <- function(object)
{
	errors <- NULL
	cls_ok <- vapply(object@listData, is, logical(1L),
		class2=object@elementType)
	if ( !all(cls_ok) ) {
		errors <- c(errors, paste0("elements must be of type ",
			sQuote(object@elementType)))
	}
	if ( is.null(errors) ) TRUE else errors
}

setValidity("ResultsList", .valid_ResultsList)

ResultsList <- function(..., mcols = NULL)
{
	if ( ...length() == 1L && is(..1, "list_OR_List") ) {
		if ( is(..1, "List") ) {
			x <- as(..1, "SimpleList")
		} else {
			x <- SimpleList(..1)
		}
	} else {
		x <- SimpleList(...)
	}
	new("ResultsList", x, elementMetadata=mcols,
		elementType=class(x[[1L]])[1L])
}

setMethod("show", "ResultsList",
	function(object) {
		callNextMethod()
		cat("model:", object@elementType, "\n")
		if ( !is.null(mcols(object)) ) {
			n <- 10L
			x <- as.data.frame(mcols(object))
			print(head(x, n=n))
			if ( nrow(x) > n )
				cat("... and", nrow(x) - n, "more results\n")
		}
	})

setMethod("fitted", "ResultsList",
	function(object, ..., simplify = TRUE)
{
	ans <- lapply(object, fitted, ...)
	if ( simplify ) {
		if ( length(ans) > 1L ) {
			if ( is.factor(ans[[1L]]) ) {
				as.data.frame(ans, check.names=FALSE)
			} else {
				simplify2array(ans)
			}
		} else {
			ans[[1L]]
		}
	} else {
		ans
	}
})

setMethod("predict", "ResultsList",
	function(object, ..., simplify = TRUE)
{
	ans <- lapply(object, predict, ...)
	if ( simplify ) {
		if ( length(ans) > 1L ) {
			if ( is.factor(ans[[1L]]) ) {
				as.data.frame(ans, check.names=FALSE)
			} else {
				simplify2array(ans)
			}
		} else {
			ans[[1L]]
		}
	} else {
		ans
	}
})

setMethod("topFeatures", "ResultsList",
	function(object, ...)
{
	lapply(object, topFeatures, ...)
})

## Plotting

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


