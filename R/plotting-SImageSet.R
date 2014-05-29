
setMethod("plot",
	signature = c(x = "SImageSet", y = "missing"),
	function(x, formula = ~ Feature,
		pixel,
		pixel.groups,
		groups = NULL,
		superpose = FALSE,
		fun = mean,
		subset = TRUE,
		lattice = FALSE,
		...)
	{
		if ( missing(pixel) ) stop("'pixel' must be specified")
		# add Feature to features for default plotting
		if ( !"Feature" %in% featureNames(x) ) fData(x)[["Feature"]] <- 1:dim(x)[[1]]
		# evaluated with respect to pData
		pixel <- eval(substitute(pixel), envir=pData(x), enclos=parent.frame(2))
		# evaluated with respect to fData
		groups <- eval(substitute(groups), envir=fData(x), enclos=parent.frame(2))
		subset <- eval(substitute(subset), envir=fData(x), enclos=parent.frame(2))
		# set up pixel.groups
		if ( missing(pixel.groups) ) {
			pixel.groups <- factor(rep(TRUE, length(pixel)))
			missing.pixel.groups <- TRUE
		} else {
			pixel.groups <- eval(substitute(pixel.groups),
				envir=pData(x), enclos=parent.frame(2))
			if ( length(pixel) != length(pixel.groups) )
				pixel.groups <- pixel.groups[.match.pixel(pixel, x)]
			pixel.groups <- as.factor(pixel.groups)
			missing.pixel.groups <- FALSE
		}
		# parse formula and set up model for plotting
		model <- .parsePlotFormula(formula, object=x, enclos=parent.frame(2))
		# calculate the plotting values and their conditioning variables
		if ( is.null(model$left) ) {
			values <- .calculatePlotValues(x, fun=fun, pixel=pixel,
				pixel.groups=pixel.groups, condition=model$condition,
				missing.pixel.groups=missing.pixel.groups)
			condition <- do.call(expand.grid, c(list(.pixel.groups=levels(pixel.groups)),
				lapply(model$condition, function(cond) levels(as.factor(cond)))))
		} else {
			values <- matrix(unlist(model$left), nrow=length(model$left[[1]]))
			condition <- data.frame(.value.groups=factor(seq_along(model$left),
				labels=make.names(names(model$left))))
		}
		nobs <- prod(dim(values)[-length(dim(values))])
		ncond <- nrow(condition)
		# branch for base or lattice graphics
		if ( lattice ) {
			# STILL NEED TO IMPLEMENT for groups=TRUE && superpose=FALSE
			# set up the lattice data
			condition <- sapply(condition, function(var) rep(var, each=nobs))
			data <- data.frame(.values=as.numeric(values), condition, row.names=NULL)
			data[[names(model$right)]] <- rep(unlist(model$right), ncond)
			# set up the groups and subset
			subset <- rep(subset, length.out=nrow(data))
			if ( superpose && is.null(groups) ) {
				groups <- data$.pixel.groups
			} else if ( !is.null(groups) ) {
				groups <- rep(groups, length.out=nrow(data))
			}
			# set up the lattice formula
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
			# plot it with lattice
			xyplot(fm, data=data, groups=groups, subset=subset, ...)
		} else {
			# STILL NEED TO IMPLEMENT for conditioning and grouping variables
			# set up the conditioning and sample variables
			data <- expand.grid(lapply(dim(imageData(x))[-1], seq_len))
			if ( !"sample" %in% names(data) ) {
				data$sample <- factor(1)
			} else {
				data$sample <- as.factor(data$sample)
			}
			# stop if conditioning variables found
			if ( ncond > 1 ) stop("conditioning variables not allowed for lattice = FALSE")
			# set up the x coordinate
			xs <- unlist(model$right)
			# loop through conditions
			for ( i in ncol(values) ) {
				y <- values[,i]
				# plot with base graphics
				plot(xs, y, ...)
			}
		}		
	})

setMethod("plot",
	signature = c(x = "SImageSet", y = "formula"),
	function(x, y, ...) {
		plot(x, formula=y, ...)
	})

setMethod("plot",
	signature = c(x = "MSImageSet", y = "missing"),
	function(x, formula = ~mz, ...) {
		callNextMethod(x, formula=formula, ...)
	})

setMethod("image",
	signature = c(x = "SImageSet"),
	function(x, formula = ~ x * y,
		feature,
		feature.groups,
		groups = NULL,
		superpose = FALSE,
		fun = mean,
		subset = TRUE,
		lattice = FALSE,
		...)
	{
		if ( missing(feature) ) stop("'feature' must be specified")
		# evaluated with respect to fData
		feature <- eval(substitute(feature), envir=fData(x), enclos=parent.frame(2))
		# evaluated with respect to pData
		groups <- eval(substitute(groups), envir=pData(x), enclos=parent.frame(2))
		if ( !is.null(groups) ) groups <- rep(groups, length.out=dim(x)[2])
		subset <- eval(substitute(subset), envir=pData(x), enclos=parent.frame(2))
		if ( !is.null(subset) ) subset <- rep(subset, length.out=dim(x)[2])
		# set up feature.groups
		if ( missing(feature.groups) ) {
			feature.groups <- factor(rep(TRUE, length(feature)))
			missing.feature.groups <- TRUE
		} else {
			feature.groups <- eval(substitute(feature.groups),
				envir=fData(x), enclos=parent.frame(2))
			if ( length(feature) != length(feature.groups))
				feature.groups <- feature.groups[.match.feature(feature, x)]
			feature.groups <- as.factor(feature.groups)
			missing.feature.groups <- FALSE
		}
		# parse formula and set up model for plotting
		model <- .parseImageFormula(formula, object=x, enclos=parent.frame(2))
		# calculate the plotting values and their conditioning variables
		if ( is.null(model$left) ) {
			values <- .calculateImageValues(x, fun=fun, feature=feature,
				feature.groups=feature.groups, condition=model$condition,
				missing.feature.groups=missing.feature.groups)
			condition <- do.call(expand.grid, c(list(.feature.groups=levels(feature.groups)),
				lapply(model$condition, function(cond) levels(as.factor(cond)))))
		} else {
			values <- matrix(unlist(model$left), nrow=length(model$left[[1]]))
			condition <- data.frame(.value.groups=factor(seq_along(model$left),
				labels=make.names(names(model$left))))
		}
		# shape image values into matrix with one image per column, includes NAs
		values <- .reshapeImageValues(values, x, groups=groups, subset=subset, lattice=lattice)
		nobs <- prod(dim(values)[-length(dim(values))])
		ncond <- nrow(condition)
		# branch for base or lattice graphics
		if ( lattice ) {
			# STILL NEED TO IMPLEMENT for groups=TRUE && superpose=FALSE
			# set up the lattice data
			condition <- sapply(condition, function(var) rep(var, each=nobs))
			data <- data.frame(.values=as.numeric(values), condition, row.names=NULL)
			data[coordLabels(x)] <- expand.grid(lapply(dim(imageData(x))[-1], seq_len))
			# set up the groups and subset
			subset <- rep(subset[positionArray(imageData(x))], ncond)
			if ( superpose && is.null(groups) ) {
				groups <- rep(levels(feature.groups), each=nobs)
			} else if ( !is.null(groups) ) {
				groups <- rep(groups[positionArray(imageData(x))], ncond)
			}
			# remove NAs so groups and subset work
			nas <- is.na(values)
			data <- data[!nas,,drop=FALSE]
			subset <- subset[!nas]
			groups <- groups[!nas]
			# set up the lattice formula
			fm.side <- paste(".values ~", paste(names(model$right), collapse="*"))
			fm.cond <- NULL
			if ( !superpose && !missing.feature.groups )
				fm.cond <- c(fm.cond, ".feature.groups")
			if ( !is.null(model$condition) )
				fm.cond <- c(fm.cond, make.names(names(model$condition)))
			if ( !is.null(model$left) )
				fm.cond <- c(fm.cond, ".value.groups")
			if ( !all(coordLabels(x) %in% names(model$right)) )
				fm.cond <- c(fm.cond, coordLabels(x)[!coordLabels(x) %in% names(model$right)])
			if ( !is.null(fm.cond) ) fm.cond <- paste(fm.cond, collapse="*")
			fm <- as.formula(paste(c(fm.side, fm.cond), collapse="|"))
			# plot it with lattice
			levelplot(fm, data=data, groups=groups, subset=subset, ...)
		} else {
			# STILL NEED TO IMPLEMENT for conditioning and grouping variables
			# set up the conditioning and sample variables
			data <- expand.grid(lapply(dim(imageData(x))[-1], seq_len))
			if ( !"sample" %in% names(data) ) {
				data$sample <- factor(1)
			} else {
				data$sample <- as.factor(data$sample)
			}
			# stop if conditioning variables found
			if ( ncond > 1 ) stop("conditioning variables not allowed for lattice = FALSE")
			# calculate the total number of plotting layers required
			nsamples <- length(levels(data$sample))
			nlayers <- ncond * nsamples
			# set up the x-y coordinates for reshaping z matrix
			xylim <- dim(imageData(x))[names(model$right)]
			xtotticks <- xylim[[1]] * nlayers # total x ticks across all image layers
			ytotticks <- xylim[[2]] * nlayers # total y ticks across all image layers
			xs <- seq(from=1, to=xylim[[1]], length.out=nrow(data) / ytotticks)
			ys <- seq(from=1, to=xylim[[2]], length.out=nrow(data) / xtotticks)
			zdim <- c(length(xs), length(ys))
			# loop through conditions and then samples
			for ( i in ncol(values) ) {
				for ( j in levels(data$sample) ) {
					z <- values[data$sample == j]
					dim(z) <- zdim
					# plot with base graphics
					image(xs, ys, z, ...)
				}
			}
		}
	})

## Reorders feature vector values for plotting an image
.reshapeImageValues <- function(values, object, groups, subset, lattice) {
	if ( !lattice && !is.null(groups) ) {
		nas <- rep(NA, nrow(values))
		newdim <- c(nrow(values), ncol(values) * length(levels(groups)))
		values <- apply(values, 2, function(x) {
			sapply(levels(groups), function(g) ifelse(groups==g, x, nas))
		})
		dim(values) <- newdim
	}
	subsetPositions <- positionArray(imageData(object))
	subsetPositions[!subset] <- NA
	retval <- apply(values, 2, function(x) x[subsetPositions])
	if ( !is.matrix(retval) )
		retval <- as.matrix(retval)
	retval
}

.calculatePlotValues <- function(object, fun, pixel, pixel.groups,
	condition, missing.pixel.groups)
{
	pixel <- .match.pixel(pixel, object)
	groups <- rep(TRUE, length(pixel))
	if ( !is.null(condition) ) {
		condition <- lapply(condition, function(cond) cond[pixel])
		groups <- do.call(interaction, condition)
	}
	if ( !missing.pixel.groups ) {
		if ( length(pixel.groups) != length(pixel) )
			pixel.groups <- pixel.groups[pixel]
		groups <- interaction(pixel.groups, groups)
	}
	.fastFeatureApply(object, fun=fun, pixel=pixel, pixel.groups=groups)
}

.calculateImageValues <- function(object, fun, feature, feature.groups,
	condition, missing.feature.groups)
{
	feature <- .match.feature(feature, object)
	groups <- rep(TRUE, length(feature))
	if ( !is.null(condition) ) {
		condition <- lapply(condition, function(cond) cond[feature])
		groups <- do.call(interaction, condition)
	}
	if ( !missing.feature.groups ) {
		if ( length(feature.groups) != length(feature) )
			feature.groups <- feature.groups[feature]
		groups <- interaction(feature.groups, groups)
	}
	.fastPixelApply(object, fun=fun, feature=feature, feature.groups=groups)
}

.fastFeatureApply <- function(object, fun, pixel, pixel.groups) {
	x <- iData(object)[,pixel,drop=FALSE]
	pixel.groups <- as.factor(pixel.groups)
	if ( length(pixel) == 1 ) {
		x <- as.matrix(x)
	} else if ( length(levels(pixel.groups)) == 1 ) {
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
	} else if ( length(levels(feature.groups)) == 1 ) {
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

