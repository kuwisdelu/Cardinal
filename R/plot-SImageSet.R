
#### Plotting for SImageSet ####

setMethod("plot",
	signature = c(x = "SImageSet", y = "missing"),
	function(x, formula = ~ Feature,
		pixel,
		pixel.groups,
		groups = NULL,
		superpose = FALSE,
		strip = TRUE,
		key = FALSE,
		fun = mean,
		...,
		xlab,
		xlim,
		ylab,
		ylim,
		layout,
		type = 'l',
		col = "black",
		subset = TRUE,
		lattice = FALSE)
	{
		.Deprecated_Cardinal1()
		# add Feature to features for default plotting
		if ( !"Feature" %in% featureNames(x) ) fData(x)[["Feature"]] <- 1:dim(x)[1]
		# parse formula and set up model for plotting
		model <- .parsePlotFormula(formula, object=x, enclos=environment(formula))
		if ( missing(pixel) && is.null(model$left) )
			.stop("plot: 'pixel' must be specified")
		# evaluated with respect to pData
		if ( is.null(model$left) ) {
			pixel <- tryCatch(eval(substitute(pixel), envir=pData(x),
				enclos=environment(formula)), error = function(e) eval(pixel))
			pixel <- pixels(x)[pixel]
		} else {
			pixel <- seq_along(model$left)
		}
		# evaluated with respect to fData
		groups <- tryCatch(eval(substitute(groups), envir=fData(x),
			enclos=environment(formula)), error = function(e) eval(groups))
		if ( !is.null(groups) )
			groups <- as.factor(groups)
		subset <- tryCatch(eval(substitute(subset), envir=fData(x),
			enclos=environment(formula)), error = function(e) eval(subset))
		# set up pixel.groups
		if ( missing(pixel.groups) ) {
			pixel.groups <- factor(rep(TRUE, length(pixel)))
			missing.pixel.groups <- TRUE
		} else {
			pixel.groups <- tryCatch(eval(substitute(pixel.groups),
				envir=pData(x), enclos=environment(formula)),
				error = function(e) eval(pixel.groups))
			if ( is.null(pixel.groups) ) {
				pixel.groups <- factor(rep(TRUE, length(pixel)))
				missing.pixel.groups <- TRUE
			} else {
				if ( length(pixel) != length(pixel.groups) )
					pixel.groups <- pixel.groups[pixel]
				pixel.groups <- as.factor(pixel.groups)
				missing.pixel.groups <- FALSE
			}
		}
		# calculate the plotting values and their conditioning variables
		if ( is.null(model$left) ) {
			values <- .calculatePlotValues(x, fun=fun, pixel=pixel,
				pixel.groups=pixel.groups, condition=model$condition,
				missing.pixel.groups=missing.pixel.groups)
			if ( is.null(model$condition) ) {
				condition <- data.frame(.pixel.groups=pixel.groups)
			} else {
				condition <- data.frame(.pixel.groups=pixel.groups,
					lapply(model$condition, function(cond) cond[pixel]))
			}
		} else {
			values <- matrix(unlist(model$left), nrow=length(model$left[[1]]))
			condition <- data.frame(.value.groups=factor(seq_along(model$left),
				labels=make.names(names(model$left))))
		}
		# needed to remove missing levels and correct ordering of conditions
		condition <- unique(condition)
		condition <- condition[do.call(order, rev(condition)),,drop=FALSE]
		# set up plotting parameters
		if ( missing(xlim) )
			xlim <- range(model$right, na.rm=TRUE)
		if ( missing(xlab) )
			xlab <- .format.plot.label(names(model$right))
		if ( missing(ylim) )
			ylim <- range(values, na.rm=TRUE)
		if ( missing(ylab) ) {
			if ( is.null(model$left) ) {
				ylab <- "Intensity"
			} else {
				ylab <- names(model$left)
			}
			ylab <- .format.plot.label(ylab)
		}
		if ( missing(layout) )
			layout <- NULL
		# set up the lattice data
		nobs <- nrow(values)
		ncond <- nrow(condition)
		cond <- lapply(condition, function(var) rep(var, each=nobs))
		data <- data.frame(.values=as.numeric(values), cond, row.names=NULL)
		data[[names(model$right)]] <- rep(unlist(model$right), ncond)
		# set up the groups and subset
		subset <- rep(subset, length.out=nrow(data))
		if ( superpose && is.null(groups) ) {
			if ( is.null(data$.pixel.groups) ) {
				groups <- data$.value.groups
			} else {
				groups <- data$.pixel.groups
			}
			groupkey <- list(text=levels(groups), col=col)
		} else if ( !is.null(groups) ) {
			groups <- factor(rep(groups, ncond) ,levels=levels(groups))
			groupkey <- list(text=levels(groups), col=col)
		} else {
			groupkey <- NULL
		}
		if ( isTRUE(key) ) {
			key <- groupkey
		} else if ( is.list(key) ) {
			key <- key
		} else {
			key <- NULL
		}
		# set up the plotting formula
		fm.side <- paste(".values ~", names(model$right))
		fm.cond <- NULL
		if ( !superpose && !missing.pixel.groups )
			fm.cond <- c(fm.cond, ".pixel.groups")
		if ( !is.null(model$condition) )
			fm.cond <- c(fm.cond, names(model$condition))
		if ( !is.null(model$left) )
			fm.cond <- c(fm.cond, ".value.groups")
		if ( !is.null(fm.cond) ) fm.cond <- paste(fm.cond, collapse="*")
		fm <- as.formula(paste(c(fm.side, fm.cond), collapse="|"))
		# branch for base or lattice graphics
		if ( lattice ) {
			# set up key
			if ( !is.null(key) ) {
				key <- list(text=list(key$text),
					rectangles=list(col=key$col[1:length(key$text)]),
					columns=min(5, length(key$text)))
			}
			# plot it with lattice
			xyplot(fm, data=data, groups=groups, subset=subset,
				xlab=xlab, xlim=xlim, ylab=ylab, ylim=ylim, type=type,
				col=col, key=key, strip=strip, layout=rev(layout),
				panel=function(x, y, col, ...) {
					panel.abline(h=0, lwd=0.2)
					panel.xyplot(x, y, col=col, ...)
				}, ...)
		} else {
			# setup plotting layout
			if ( !is.null(layout) ) .setup.layout(rev(layout))
			# check which conditions should create new plots
			if ( superpose ) {
				superposed <- which(names(condition) == ".pixel.groups")
				if ( ncol(condition) > 1 ) {
					superposed <- duplicated(condition[,-superposed,drop=FALSE])
				} else {
					superposed <- c(FALSE, rep(TRUE, ncond - 1))
				}
			} else {
				superposed <- logical(ncond)
			}
			# set up the plotting coordinates
			xs <- unlist(model$right)
			# loop through conditions
			for ( ci in seq_len(nrow(condition)) ) {
				add <- superposed[ci]
				last <- c(!superposed[-1], TRUE)[ci]
				subscripts <- subset_rows(data, condition[ci,,drop=FALSE])
				ys <- data[subscripts, ".values"]
				ys[!subset[subscripts]] <- NA
				if ( !all(is.na(ys)) ) {
					if ( is.null(groups) ) {
						plot(0, 0, type='n',
							xlab=xlab, xlim=xlim,
							ylab=ylab, ylim=ylim, ...)
						abline(h=0, lwd=0.2)
						for ( tpi in type ) {
							points(xs, ys, type=tpi, col=col, ...)
						}
					} else {
						if ( !add ) {
							plot(0, 0, type='n', xlab=xlab, xlim=xlim,
								ylab=ylab, ylim=ylim, ...)
							abline(h=0, lwd=0.2)
						}
						subgroups <- which(levels(groups) %in% groups[subscripts])
						for ( gi in subgroups ) {
							ys.g <- ys
							not.g <- groups[subscripts] != levels(groups)[gi]
							not.g[is.na(not.g)] <- TRUE
							ys.g[not.g] <- NA
							for ( tpi in type ) {
								points(xs, ys.g, type=tpi, col=col[gi], ...)
							}
						}
					}
				}
				if ( last && any(subset[subscripts], na.rm=TRUE) ) {
					if ( strip && length(fm.cond != 0 ) ) {
						labels <- names(condition)
						if ( superpose || missing.pixel.groups )
							labels <- setdiff(labels, ".pixel.groups")
						strip.labels <- sapply(condition[ci,labels,drop=FALSE], as.character)
						legend("top", legend=strip.labels, x.intersp=0,
							bg=rgb(1, 1, 1, 0.75), cex=0.8)
					}
					if ( !is.null(key) ) {
						legend("topright", legend=key$text, fill=key$col,
							bg=rgb(1, 1, 1, 0.75))
					}
				}
			}
		}		
	})

setMethod("plot",
	signature = c(x = "SImageSet", y = "formula"),
	function(x, y, ...) {
		plot(x, formula = y, ...)
	})

.calculatePlotValues <- function(object, fun, pixel, pixel.groups,
	condition, missing.pixel.groups)
{
	pixel <- pixels(object)[pixel]
	groups <- rep(TRUE, length(pixel))
	if ( !is.null(condition) ) {
		condition <- lapply(condition, function(cond) cond[pixel])
		groups <- do.call(interaction, c(condition, list(drop=TRUE)))
	}
	if ( !missing.pixel.groups ) {
		if ( length(pixel.groups) != length(pixel) )
			pixel.groups <- pixel.groups[pixel]
		groups <- interaction(pixel.groups, groups, drop=TRUE)
	}
	.fastFeatureApply(object, fun=fun, pixel=pixel, pixel.groups=groups)
}

.fastFeatureApply <- function(object, fun, pixel, pixel.groups) {
	x <- iData(object)[,pixel,drop=FALSE]
	pixel.groups <- as.factor(pixel.groups)
	if ( length(pixel) == 1 ) {
		x <- as.matrix(x)
	} else if ( nlevels(pixel.groups) == 1 ) {
		x <- as.matrix(apply(x, 1, fun))
	} else {
		x <- sapply(levels(pixel.groups), function(g) {
			apply(x[,pixel.groups==g,drop=FALSE], 1, fun)
		})
	}
	x
}
