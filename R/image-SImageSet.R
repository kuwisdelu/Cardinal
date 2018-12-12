
#### Image plotting for SImageSet ####

setMethod("image",
	signature = c(x = "SImageSet"),
	function(x, formula = ~ x * y,
		feature,
		feature.groups,
		groups = NULL,
		superpose = FALSE,
		strip = TRUE,
		key = superpose,
		fun = mean,
		normalize.image = c("none", "linear"),
		contrast.enhance = c("none", "suppression", "histogram"),
	    smooth.image = c("none", "gaussian", "adaptive"),
		...,
		xlab,
		xlim,
		ylab,
		ylim,
		zlab,
		zlim,
		layout,
		asp = 1,
		col = rainbow(nlevels(groups)),
		col.regions = intensity.colors(100),
		colorkey = !is3d,
		subset = TRUE,
		lattice = FALSE)
	{
		# parse formula and set up model for plotting
		model <- .parseImageFormula(formula, object=x, enclos=environment(formula))
		if ( missing(feature) && is.null(model$left) )
			.stop("image: either 'feature' or LHS to formula must be specified")
		if ( length(model$right) >= 3 ) {
			is3d <- TRUE
		} else {
			is3d <- FALSE
		}
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
		subsetPositions[!subset[subsetPositions]] <- NA
		subsetPositions <- aperm(subsetPositions, perm=coordNames)
		values <- values[subsetPositions,,drop=FALSE]
		dim(values) <- c(dim(subsetPositions), nrow(condition))
		names(dim(values)) <- c(coordNames, character(1))
		subset <- subset[subsetPositions]
		groups <- groups[subsetPositions]
		# perform image processing (contrast enhancement + spatial smoothing)
		contrast.enhance <- contrast.enhance.method(contrast.enhance)
		smooth.image <- smooth.image.method(smooth.image)
		normalize.image <- normalize.image.method(normalize.image)
		if ( is3d ) {
			# if ( isTRUE(getOption("Cardinal.3D.normalize.by.sample")) ) {
			# 	names3d <- names(dim(values))
			# 	dim3d <- dim(values)
			# 	values <- apply(values, which(!names(dim(values)) %in% c("x","y")),
			# 		function(x) normalize.image(contrast.enhance(x)))
			# 	dim(values) <- dim3d[c("x","y",names3d[which(!names3d %in% c("x","y"))])]
			# 	perm3d <- seq_along(dim(values))
			# 	names(perm3d) <- names(dim(values))
			# 	values <- aperm(values, perm=perm3d[names3d])
			# } else {
				values <- apply(values, seq(from=4, to=length(dim(values)), by=1),
					function(x) normalize.image(contrast.enhance(x)))
			# }
		} else {
			values <- apply(values, seq(from=3, to=length(dim(values)), by=1),
				function(x) normalize.image(smooth.image(contrast.enhance(x))))
		}
		# set up plotting parameters
		if ( missing(xlim) )
			xlim <- range(model$right[[1]], na.rm=TRUE) + c(-0.5, 0.5)
		if ( missing(xlab) )
			xlab <- .format.plot.label(names(model$right)[[1]], character.only=is3d && !lattice)
		if ( missing(ylim) )
			ylim <- range(model$right[[2]], na.rm=TRUE) + c(-0.5, 0.5)
		if ( missing(ylab) )
			ylab <- .format.plot.label(names(model$right)[[2]], character.only=is3d && !lattice)
		if ( missing(zlim) ) {
			if ( is3d ) {
				zlim <- range(model$right[[3]], na.rm=TRUE) + c(-0.5, 0.5)
			} else {
				zlim <- range(values, na.rm=TRUE)
			}
		}
		if ( missing(zlab) && is3d )
			zlab <- .format.plot.label(names(model$right)[[3]], character.only=is3d && !lattice)
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
			if ( is.null(data$.feature.groups) ) {
				groups <- data$.value.groups
			} else {
				groups <- data$.feature.groups
			}
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
			if ( is3d )
				.stop("image: 3D plotting not yet implemented for lattice graphics")
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
			levelplot(fm, data=data, groups=groups, subset=subset, layout=rev(layout),
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
							col.g <- alpha.colors(col[gi], length(col.regions))
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
						paste0(nm, " = ", seq_len(dim(imageData(x))[[nm]]))
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
			if ( !is.null(layout) ) .setup.layout(rev(layout))
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
			if ( is3d ) {
				zs <- seq_len(max(model$right[[3]]))
			} else {
				zs <- integer()
			}
			# loop through conditions + dimensions
			for ( ci in seq_len(ncond) ) {
				add <- superposed[ci]
				last <- c(!superposed[-1], TRUE)[ci]
				subscripts <- seq(from=1 + (ci - 1) * nrow(data) / ncond,
					by=1, length.out=nrow(data) / ncond)
				vals <- data[subscripts, ".values"]
				if ( is3d ) {
					dim(vals) <- c(length(xs), length(ys), length(zs))
				} else {
					dim(vals) <- c(length(xs), length(ys))
				}
				if ( !all(is.na(vals)) ) {
					if ( is.null(groups) ) {
						if ( is3d ) {
							image3d(xs, ys, zs, vals, xlab=xlab, xlim=xlim,
								ylab=ylab, ylim=ylim, zlab=zlab, zlim=zlim,
								col=col.regions, ...)
						} else {
							image(xs, ys, vals, xlab=xlab, xlim=xlim,
								ylab=ylab, ylim=rev(ylim), zlim=zlim,
								asp=asp, col=col.regions, ...)
						}
						if ( is.list(colorkey) ) {
							if ( is3d ) {
								col.range <- range(values, na.rm=TRUE)
							} else {
								col.range <- zlim
							}
							legend("topright",
								legend=c(
									round(col.range[2], 2),
									rep(NA, length(col.regions)-2),
									round(col.range[1], 2)),
								col=rev(col.regions),
								bg=rgb(1, 1, 1, 0.75),
								y.intersp=0.1,
								cex=0.6,
								lwd=2)
						}
					} else {
						subgroups <- which(levels(groups) %in% groups[subscripts])
						for ( gi in subgroups ) {
							vals.g <- vals
							add.g <- add || gi != subgroups[1]
							col.g <- alpha.colors(col[gi], length(col.regions))
							not.g <- groups[subscripts] != levels(groups)[gi]
							not.g[is.na(not.g)] <- TRUE
							vals.g[not.g] <- NA
							if ( all(is.na(vals.g)) )
								next
							if ( is3d ) {
								image3d(xs, ys, zs, vals.g, xlab=xlab, xlim=xlim,
									ylab=ylab, ylim=ylim, zlab=zlab, zlim=zlim,
									col=col.g, add=add.g, ...)
							} else {
								image(xs, ys, vals.g, xlab=xlab, xlim=xlim,
									ylab=ylab, ylim=rev(ylim), zlim=zlim,
									asp=asp, col=col.g, add=add.g, ...)
							}
						}
					}
				}
				if ( last && any(subset[subscripts], na.rm=TRUE) ) {
					if ( strip && length(fm.cond != 0 ) ) {
						labels <- names(condition)
						if ( superpose || missing.feature.groups )
							labels <- setdiff(labels, ".feature.groups")
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

setMethod("image3D",
	signature = c(x = "SImageSet"),
	function(x, formula = ~ x * y * z, ...)
	{
		image(x, formula=formula, ...)
	})



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
