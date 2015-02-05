
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
			xlab <- .format.label(names(model$right))
		if ( missing(ylim) )
			ylim <- range(values, na.rm=TRUE)
		if ( missing(ylab) ) {
			if ( is.null(model$left) ) {
				ylab <- "Intensity"
			} else {
				ylab <- names(model$left)
			}
			ylab <- .format.label(ylab)
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
			groups <- data$.pixel.groups
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
		# debugging
		if ( isTRUE(getOption("Cardinal.debug.plotting")) ) browser()
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
				col=col, key=key, strip=strip, layout=layout,
				panel=function(x, y, col, ...) {
					panel.abline(h=0, lwd=0.2)
					panel.xyplot(x, y, col=col, ...)
				}, ...)
		} else {
			# setup plotting layout
			if ( !is.null(layout) ) .setup.layout(layout)
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
				subscripts <- subrows(data, condition[ci,,drop=FALSE])
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
						strip.labels <- as.character(unlist(condition[ci,labels,drop=FALSE]))
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

setMethod("image",
	signature = c(x = "SImageSet"),
	function(x, formula = ~ x * y,
		feature,
		feature.groups,
		groups = NULL,
		superpose = FALSE,
		strip = TRUE,
		key = FALSE,
		fun = mean,
		normalize.image = c("none", "linear"),
		contrast.enhance = c("none", "suppression", "histogram"),
	    smooth.image = c("none", "gaussian", "adaptive"),
		...,
		xlab,
		xlim,
		ylab,
		ylim,
		zlim,
		layout,
		asp = 1,
		col = rainbow(nlevels(feature.groups)),
		col.regions = intensity.colors(100),
		colorkey = TRUE,
		subset = TRUE,
		lattice = FALSE)
	{
		# parse formula and set up model for plotting
		model <- .parseImageFormula(formula, object=x, enclos=environment(formula))
		if ( missing(feature) && is.null(model$left) )
			.stop("image: either 'feature' or LHS to formula must be specified")
		# evaluated with respect to fData
		if ( is.null(model$left) ) {
			feature <- tryCatch(eval(substitute(feature), envir=fData(x),
				enclos=environment(formula)), error = function(e) eval(feature))
			feature <- features(x)[feature]
		} else {
			feature <- seq_along(model$left)
		}
		# evaluated with respect to pData
		groups <- tryCatch(eval(substitute(groups), envir=pData(x),
			enclos=environment(formula)), error = function(e) eval(groups))
		if ( !is.null(groups) ) {
			groups <- as.factor(groups)
			groups <- rep(groups, length.out=dim(x)[2])
		}
		subset <- tryCatch(eval(substitute(subset), envir=pData(x),
			enclos=environment(formula)), error = function(e) eval(subset))
		if ( !is.null(subset) )
			subset <- rep(subset, length.out=dim(x)[2])
		# set up feature.groups
		if ( missing(feature.groups) ) {
			feature.groups <- factor(rep(TRUE, length(feature)))
			missing.feature.groups <- TRUE
		} else {
			feature.groups <- tryCatch(eval(substitute(feature.groups),
				envir=fData(x), enclos=environment(formula)),
				error = function(e) eval(feature.groups))
			if ( is.null(feature.groups) ) {
				feature.groups <- factor(rep(TRUE, length(feature)))
				missing.feature.groups <- TRUE
			} else {
				if ( length(feature) != length(feature.groups))
					feature.groups <- feature.groups[feature]
				feature.groups <- as.factor(feature.groups)
				missing.feature.groups <- FALSE
			}
		}
		# calculate the plotting values and their conditioning variables
		if ( is.null(model$left) ) {
			values <- .calculateImageValues(x, fun=fun, feature=feature,
				feature.groups=feature.groups, condition=model$condition,
				missing.feature.groups=missing.feature.groups)
			if ( is.null(model$condition) ) {
				condition <- data.frame(.feature.groups=feature.groups)
			} else {
				condition <- data.frame(.feature.groups=feature.groups,
					lapply(model$condition, function(cond) cond[feature]))
			}
		} else {
			values <- matrix(as.numeric(unlist(model$left)),
				nrow=length(model$left[[1]]))
			condition <- data.frame(.value.groups=factor(seq_along(model$left),
				labels=make.names(names(model$left))))
		}
		# needed to remove missing levels and correct ordering of conditions
		condition <- unique(condition)
		condition <- condition[do.call(order, rev(condition)),,drop=FALSE]
		# shape values into matrix with 1 hyper-image per column, includes NAs
		coordNames <- union(names(model$right), names(coord(x)))
		subsetPositions <- positionArray(imageData(x))
		subset <- subset[subsetPositions]
		groups <- groups[subsetPositions]
		subsetPositions[!subset] <- NA
		subsetPositions <- aperm(subsetPositions, perm=coordNames)
		values <- values[subsetPositions,,drop=FALSE]
		dim(values) <- c(dim(subsetPositions), nrow(condition))
		# perform image processing (contrast enhancement + spatial smoothing)
		normalize.image <- normalize.image.method(normalize.image)
		contrast.enhance <- contrast.enhance.method(contrast.enhance)
		smooth.image <- smooth.image.method(smooth.image)
		values <- apply(values, seq(from=3, to=length(dim(values)), by=1),
			function(x) smooth.image(contrast.enhance(normalize.image(x))))
		# set up plotting parameters
		if ( missing(xlim) )
			xlim <- range(model$right[[1]], na.rm=TRUE) + c(-0.5, 0.5)
		if ( missing(xlab) )
			xlab <- .format.label(names(model$right)[[1]])
		if ( missing(ylim) )
			ylim <- range(model$right[[2]], na.rm=TRUE) + c(-0.5, 0.5)
		if ( missing(ylab) )
			ylab <- .format.label(names(model$right)[[2]])
		if ( missing(zlim) )
			zlim <- range(values, na.rm=TRUE)
		if ( is.logical(colorkey) && colorkey )
			colorkey <- list(col=col.regions)
		if ( missing(layout) )
			layout <- NULL
		# set up the plotting data
		nobs <- prod(dim(values)[-length(dim(values))])
		ncond <- nrow(condition)
		cond <- lapply(condition, function(var) rep(var, each=nobs))
		data <- data.frame(.values=as.numeric(values), cond, row.names=NULL)
		data[coordNames] <- expand.grid(lapply(coordNames, function(nm) {
			if ( is.factor(coord(x)[[nm]]) ) {
				levels(coord(x)[[nm]])
			} else {
				seq_len(dim(imageData(x))[[nm]])
			}
		}))
		# set up the groups and subset
		subset <- rep(subset, ncond)
		if ( superpose && is.null(groups) ) {
			groups <- data$.feature.groups
			groupkey <- list(text=levels(feature.groups), col=col)
		} else if ( !is.null(groups) ) {
			groups <- factor(rep(groups, ncond), levels=levels(groups))
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
		fm.side <- paste(".values ~", paste(names(model$right), collapse="*"))
		fm.cond <- NULL
		if ( !superpose && !missing.feature.groups )
			fm.cond <- c(fm.cond, ".feature.groups")
		if ( !is.null(model$condition) )
			fm.cond <- c(fm.cond, make.names(names(model$condition)))
		if ( !is.null(model$left) )
			fm.cond <- c(fm.cond, ".value.groups")
		if ( !all(names(coord(x)) %in% names(model$right)) )
			fm.cond <- c(fm.cond, setdiff(names(coord(x)), names(model$right)))
		if ( !is.null(fm.cond) ) fm.cond <- paste(fm.cond, collapse="*")
		fm <- as.formula(paste(c(fm.side, fm.cond), collapse="|"))
		# debugging
		if ( isTRUE(getOption("Cardinal.debug.plotting")) ) browser()
		# branch for base or lattice graphics
		if ( lattice ) {
			# set up key
			if ( !is.null(key) ) {
				key <- list(text=list(key$text),
					rectangles=list(col=key$col[1:length(key$text)]),
					columns=min(5, length(key$text)))
				colorkey <- FALSE
			}
			# remove NAs so groups and subset work
			nas <- is.na(values)
			data <- data[!nas,,drop=FALSE]
			subset <- subset[!nas]
			groups <- groups[!nas]
			# plot it with lattice
			levelplot(fm, data=data, groups=groups, subset=subset, layout=layout,
				xlab=xlab, xlim=xlim, ylab=ylab, ylim=rev(ylim), aspect="iso",
				at=seq(from=zlim[1], to=zlim[2], length.out=length(col.regions)),
				col.regions=col.regions, colorkey=colorkey, key=key, strip=strip,
				panel=function(x, y, z, col.regions, subscripts, ...) {
					if ( is.null(groups) ) {
						panel.levelplot(x, y, z,
							subscripts=subscripts,
							col.regions=col.regions, ...)
					} else {
						subgroups <- which(levels(groups) %in% groups[subscripts])
						for ( gi in subgroups ) {
							col.g <- alpha.colors(length(col.regions), col[gi])
							panel.levelplot(x, y, z,
								subscripts=subscripts[groups==levels(groups)[gi]],
								col.regions=col.g, ...)
						}
					}
				}, ...)
		} else {
			# pad data and condition with any missing dimensions
			if ( !all(names(coord(x)) %in% names(model$right)) ) {
				# correct data, groups, and subset
				neworder <- aperm(array(seq_len(nrow(data)), dim=dim(values)),
					perm=c(1, length(dim(values)), 2:(length(dim(values))-1)))
				data <- data[neworder,,drop=FALSE]
				subset <- subset[neworder]
				groups <- groups[neworder]
				# correct condition
				coordNeed <- setdiff(names(coord(x)), names(model$right))
				coordExtra <- expand.grid(lapply(coordNeed, function(nm) {
					if ( is.factor(coord(x)[[nm]]) ) {
						levels(coord(x)[[nm]])
					} else {
						seq_len(dim(imageData(x))[[nm]])
					}
				}))
				coordCond <- do.call("rbind", apply(coordExtra, 1, function(extra) {
					data.frame(do.call("rbind", rep(list(extra), times=nrow(condition))))
				}))
				condition <- rep(list(condition), each=nrow(coordExtra))
				condition <- do.call("rbind", condition)
				condition[coordNeed] <- coordCond
				ncond <- nrow(condition)
			}
			# setup plotting layout
			if ( !is.null(layout) ) .setup.layout(layout)
			# check which conditions should create new plots
			if ( superpose ) {
				superposed <- which(names(condition) == ".feature.groups")
				if ( ncol(condition) > 1 ) {
					superposed <- duplicated(condition[,-superposed,drop=FALSE])
				} else {
					superposed <- c(FALSE, rep(TRUE, ncond - 1))
				}
			} else {
				superposed <- logical(ncond)
			}
			# set up the plotting coordinates
			xs <- seq_len(max(model$right[[1]]))
			ys <- seq_len(max(model$right[[2]]))
			# loop through conditions + dimensions
			for ( ci in seq_len(ncond) ) {
				add <- superposed[ci]
				last <- c(!superposed[-1], TRUE)[ci]
				subscripts <- seq(from=1 + (ci - 1) * nrow(data) / ncond,
					by=1, length.out=nrow(data) / ncond)
				zs <- data[subscripts, ".values"]
				dim(zs) <- c(length(xs), length(ys))
				if ( !all(is.na(zs)) ) {
					if ( is.null(groups) ) {
						image(xs, ys, zs, xlab=xlab, xlim=xlim,
							ylab=ylab, ylim=rev(ylim), zlim=zlim,
							asp=asp, col=col.regions, ...)
						if ( is.list(colorkey) )
							legend("topright",
								legend=c(
									round(zlim[2], 2),
									rep(NA, length(col.regions)-2),
									round(zlim[1], 2)),
								col=rev(col.regions),
								bg=rgb(1, 1, 1, 0.75),
								y.intersp=0.1,
								cex=0.6,
								lwd=2)
					} else {
						subgroups <- which(levels(groups) %in% groups[subscripts])
						for ( gi in subgroups ) {
							zs.g <- zs
							add.g <- add || gi != subgroups[1]
							col.g <- alpha.colors(length(col.regions), col[gi])
							not.g <- groups[subscripts] != levels(groups)[gi]
							not.g[is.na(not.g)] <- TRUE
							zs.g[not.g] <- NA
							if ( all(is.na(zs.g)) ) next
							image(xs, ys, zs.g, xlab=xlab, xlim=xlim,
								ylab=ylab, ylim=rev(ylim), zlim=zlim,
								asp=asp, col=col.g, add=add.g, ...)
						}
					}
				}
				if ( last && any(subset[subscripts], na.rm=TRUE) ) {
					if ( strip && length(fm.cond != 0 ) ) {
						labels <- names(condition)
						if ( superpose || missing.feature.groups )
							labels <- setdiff(labels, ".feature.groups")
						strip.labels <- as.character(unlist(condition[ci,labels,drop=FALSE]))
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

setMethod("select",
	signature = c(x = "SImageSet"),
	function(x, formula = ~ x * y,
		mode = c("region", "pixel"),
		...,
		main,
		subset = TRUE,
		lattice = FALSE)
	{
		mode <- match.arg(mode)
		if ( missing(main) )
			main <- paste("Select", mode)
		if ( lattice )
			.stop("select: Selection not currently supported for lattice graphics.")
		subset2 <- tryCatch(eval(substitute(subset), envir=pData(x),
			enclos=environment(formula)), error = function(e) eval(subset))
		image(x, formula=formula, ..., main=main, subset=subset2, lattice=lattice)
		model <- .parseImageFormula(formula, object=x, enclos=environment(formula))
		if ( length(subset2) < ncol(x) )
			subset2 <- rep(subset2, length.out=ncol(x))
		.message("Select pixels and press ESC or second mouse button when done")
		if ( mode == "region" ) {
			loc <- locator(type="o", pch=20, col="white", lwd=1.5)
			if ( is.null(loc) ) return(NULL)
			coord <- coord(x)[subset2, names(model$right)]
			selected <- numeric(ncol(x))
			selected[subset2] <- point.in.polygon(coord[,1], coord[,2], loc$x, loc$y)
			selected <- selected > 0
			names(selected) <- pixelNames(x)
		} else {
			loc <- locator(type="p", pch=4, col="white")
			if ( is.null(loc) ) return(NULL)
			coord <- data.frame(round(loc$x), round(loc$y))
			names(coord) <- names(model$right)
			ok <- logical(ncol(x))
			ok[subset2] <- TRUE
			selected <- logical(ncol(x))
			selected[pixels(x, coord=coord)] <- TRUE
			selected <- selected & ok
			names(selected) <- pixelNames(x)
		}
		selected
	})

.setup.layout <- function(layout) {
	par(mar=c(3,3,2,1), mgp=c(1.5,0.5,0),
		cex.axis=0.8, cex.lab=0.8)
	layout(matrix(seq_len(prod(layout)),
		ncol=layout[1], nrow=layout[2],
		byrow=TRUE))
}

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

.calculateImageValues <- function(object, fun, feature, feature.groups,
	condition, missing.feature.groups)
{
	feature <- features(object)[feature]
	groups <- rep(TRUE, length(feature))
	if ( !is.null(condition) ) {
		condition <- lapply(condition, function(cond) cond[feature])
		groups <- do.call(interaction, c(condition, list(drop=TRUE)))
	}
	if ( !missing.feature.groups ) {
		if ( length(feature.groups) != length(feature) )
			feature.groups <- feature.groups[feature]
		groups <- interaction(feature.groups, groups, drop=TRUE)
	}
	.fastPixelApply(object, fun=fun, feature=feature, feature.groups=groups)
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

.fastPixelApply <- function(object, fun, feature, feature.groups) {
	x <- iData(object)[feature,,drop=FALSE]
	feature.groups <- as.factor(feature.groups)
	if ( length(feature) == 1 ) {
		x <- t(x)
	} else if ( nlevels(feature.groups) == 1 ) {
		x <- as.matrix(apply(x, 2, fun))
	} else {
		x <- sapply(levels(feature.groups), function(g) {
			apply(x[feature.groups==g,,drop=FALSE], 2, fun)
		})
	}
	x
}

.parsePlotFormula <- function(formula, object, enclos) {
	out <- .parseFormula(formula)
	out <- lapply(out, as.list)
	for ( i in seq_along(out$left) )
		out$left[[i]] <- eval(parse(text=out$left[[i]]),
			envir=fData(object), enclos=enclos)
	for ( i in seq_along(out$right) )
		out$right[[i]] <- eval(parse(text=out$right[[i]]),
			envir=fData(object), enclos=enclos)
	for ( i in seq_along(out$condition) )
		out$condition[[i]] <- eval(parse(text=out$condition[[i]]),
			envir=pData(object), enclos=enclos)
	out <- lapply(out, function(o) if ( length(o) == 0) NULL else o)
	out
}

.parseImageFormula <- function(formula, object, enclos) {
	out <- .parseFormula(formula)
	out <- lapply(out, as.list)
	for ( i in seq_along(out$left) )
		out$left[[i]] <- eval(parse(text=out$left[[i]]),
			envir=pData(object), enclos=enclos)
	for ( i in seq_along(out$right) )
		out$right[[i]] <- eval(parse(text=out$right[[i]]),
			envir=pData(object), enclos=enclos)
	for ( i in seq_along(out$condition) )
		out$condition[[i]] <- eval(parse(text=out$condition[[i]]),
			envir=fData(object), enclos=enclos)
	out <- lapply(out, function(o) if ( length(o) == 0) NULL else o)
	out
}

.parseSide <- function(formula) {
	if ( length(formula) == 1 ) {
		paste(formula)
	} else if ( paste(formula[[1]]) %in% c("[", "[[", "$") ) {
		deparse(formula)
	} else if ( length(formula) == 2  && paste(formula[[1]]) == "I" ) {
		paste(formula)[[2]]
	} else if ( length(formula[[2]]) == 3 ) {
		c(.parseSide(formula[[2]]), paste(formula[[3]]))
	} else if ( length(formula) == 2 ) {
		paste(paste(formula, collapse="("), ")", sep="")
	} else {
		paste(formula[-1])
	}
}

.parseFormula <- function(formula) {
	if ( length(formula) == 2 ) {
		right <- formula[[2]]
		left <- NULL
	} else if ( length(formula) == 3 ) {
		right <- formula[[3]]
		left <- formula[[2]]
	}
	if ( length(right) == 1 ) {
		condition <- NULL
		right <- right
	} else if ( length(right) == 3 && paste(right[[1]]) != "|" ) {
		condition <- NULL
		right <- right
	} else if ( length(right) == 3 && paste(right[[1]]) == "|" ) {
		condition <- right[[3]]
		right <- right[[2]]
	} else {
		condition <- NULL
	}
	if ( !is.null(left) )
		left <- .parseSide(left)
	names(left) <- left
	if ( !is.null(right) )
		right <- .parseSide(right)
	names(right) <- right
	if ( !is.null(condition) )
		condition <- .parseSide(condition)
	names(condition) <- condition
	list(left=left, right=right, condition=condition)
}

