
#### Plotting for ResultSet ####

setMethod("plot",
	signature = c(x = "ResultSet", y = "missing"),
	function(x, formula,
		model = pData(modelData(x)),
		pixel,
		pixel.groups,
		superpose = TRUE,
		strip = TRUE,
		key = superpose,
		...,
		xlab,
		ylab,
		column,
		col = if (superpose) rainbow(nlevels(pixel.groups)) else "black",
		lattice = FALSE)
	{
		if ( isTRUE(getOption("Cardinal.debug.plotting")) ) browser()
		if ( is.list(model) || any(names(model) %in% varLabels(modelData(x))) ) {
			model <- model
		} else {
			model <- pData(modelData(x))[model,,drop=FALSE]
		}
		# parse formula and result data to be plotted
		fm <- .parseFormula(formula)
		formula <- paste("~", paste(names(fm$right), collapse="*"))
		formula <- as.formula(paste(c(formula, "model"), collapse="|"))
		data <- lapply(resultData(x), function(ob) ob[[names(fm$left)]])
		# set up model data.frame
		if ( is.factor(data[[1]]) ) {
			data <- lapply(data, function(class) {
				sapply(levels(class),
					function(Ck) as.integer(class == Ck))
			})
		}
		if ( is.matrix(data[[1]]) ) {
			ntimes <- mapply(rep,
				seq_len(nrow(modelData(x))),
				sapply(data, ncol))	
		} else {
			ntimes <- seq_len(nrow(modelData(x)))
		}
		ntimes <- as.vector(unlist(ntimes))
		df <- pData(modelData(x))[ntimes,,drop=FALSE]
		df <- as.data.frame(lapply(df, as.factor))
		if ( !is.null(dim(data[[1]])) ) {
			if ( is.null(colnames(data[[1]])) ) {
				df$column <- as.vector(unlist(sapply(sapply(data, ncol),
					function(times) seq_len(times))))
			} else {
				df$column <- as.vector(unlist(sapply(data, colnames)))
			}
			df$column <- factor(df$column, levels=unique(df$column))
		} else {
			df$column <- factor(rep(TRUE, nrow(df)), labels=names(fm$left))
		}
		# set up SImageSet to be plotted
		data <- SImageSet(data=matrix(as.vector(unlist(data)),
			nrow=nrow(x)), coord=df)
		fData(data) <- fData(x)
		pData(data)$model <- factor(names(resultData(x))[ntimes],
			levels=names(resultData(x)),
			labels=names(resultData(x)))
		# get which models should be plotted
		if ( is.data.frame(model) ) {
			which <- unlist(apply(model, 1, function(par) {
				do.call("pixels", c(list(data), par))
			}))
		} else {
			which <- do.call("pixels", c(list(data), model))
		}
		data <- data[,which]
		if ( missing(column) ) {
			column <- list(column=unique(df$column))
		} else if ( is.numeric(column) ) {
			column <- list(column=levels(df$column)[column])
		} else {
			column <- list(column=as.factor(column))
		}
		if ( missing(xlab) )
			xlab <- .format.label(names(fm$right))
		if ( missing(ylab) )
			ylab <- .format.label(names(fm$left))
		if ( missing(pixel) )
			pixel <- do.call("pixels", c(list(data), column))
		if ( missing(pixel.groups) )
			pixel.groups <- factor(pData(data)$column[pixel], levels=levels(df$column))
		plot(data, formula=formula, pixel=pixel, pixel.groups=pixel.groups,
			superpose=superpose, key=key, strip=strip, col=col,
			xlab=xlab, ylab=ylab, lattice=lattice, ...)
	})

setMethod("plot",
	signature = c(x = "ResultSet", y = "formula"),
	function(x, y, ...) {
		plot(x, formula = y, ...)
	})

setMethod("image",
	signature = c(x = "ResultSet"),
	function(x, formula,
		model = pData(modelData(x)),
		feature,
		feature.groups,
		superpose = TRUE,
		strip = TRUE,
		key = superpose,
		...,
		column,
		col = if (superpose) rainbow(nlevels(feature.groups)) else "black",
		lattice = FALSE)
	{
		if ( isTRUE(getOption("Cardinal.debug.plotting")) ) browser()
		if ( is.list(model) || any(names(model) %in% varLabels(modelData(x))) ) {
			model <- model
		} else {
			model <- pData(modelData(x))[model,,drop=FALSE]
		}
		# parse formula and result data to be plotted
		fm <- .parseFormula(formula)
		formula <- paste("~", paste(names(fm$right), collapse="*"))
		formula <- as.formula(paste(c(formula, "model"), collapse="|"))
		data <- lapply(resultData(x), function(ob) ob[[names(fm$left)]])
		# set up model data.frame
		if ( is.factor(data[[1]]) ) {
			data <- lapply(data, function(class) {
				sapply(levels(class),
					function(Ck) as.integer(class == Ck))
			})
		}
		if ( is.matrix(data[[1]]) ) {
			ntimes <- mapply(rep,
				seq_len(nrow(modelData(x))),
				sapply(data, ncol))
		} else {
			ntimes <- seq_len(nrow(modelData(x)))
		}
		ntimes <- as.vector(unlist(ntimes))
		df <- pData(modelData(x))[ntimes,,drop=FALSE]
		df <- as.data.frame(lapply(df, as.factor))
		if ( !is.null(dim(data[[1]])) ) {
			if ( is.null(colnames(data[[1]])) ) {
				df$column <- as.vector(unlist(sapply(sapply(data, ncol),
					function(times) seq_len(times))))
			} else {
				df$column <- as.vector(unlist(sapply(data, colnames)))
			}
			df$column <- factor(df$column, levels=unique(df$column))
		} else {
			df$column <- factor(rep(TRUE, nrow(df)), labels=names(fm$left))
		}
		data <- SImageSet(data=matrix(as.vector(unlist(data)),
			ncol=ncol(x), byrow=TRUE), coord=coord(x))
		fData(data) <- df
		fData(data)$model <- factor(names(resultData(x))[ntimes],
			levels=names(resultData(x)),
			labels=names(resultData(x)))
		# get which models should be plotted
		if ( is.data.frame(model) ) {
			which <- unlist(apply(model, 1, function(par) {
				do.call("features", c(list(data), par))
			}))
		} else {
			which <- do.call("features", c(list(data), model))
		}
		data <- data[which,]
		if ( missing(column) ) {
			column <- list(column=unique(df$column))
		} else if ( is.numeric(column) ) {
			column <- list(column=levels(df$column)[column])
		} else {
			column <- list(column=as.factor(column))
		}
		if ( missing(feature) )
			feature <- do.call("features", c(list(data), column))
		if ( missing(feature.groups) )
			feature.groups <- factor(fData(data)$column[feature], levels=levels(df$column))
		image(data, formula=formula, feature=feature, feature.groups=feature.groups,
			superpose=superpose, key=key, strip=strip, col=col,
			lattice=lattice, ...)
	})

#### Plotting for CrossValidated ####

setMethod("plot",
	signature = c(x = "CrossValidated", y = "missing"),
	function(x, fold = 1:length(x), layout, ...)
	{
		if ( !missing(layout) ) .setup.layout(layout)
		for ( i in fold ) plot(resultData(x)[[i]], ...)
	})

setMethod("image",
	signature = c(x = "CrossValidated"),
	function(x, fold = 1:length(x), layout, ...)
	{
		if ( !missing(layout) ) .setup.layout(layout)
		for ( i in fold ) image(resultData(x)[[i]], ...)
	})

#### Plotting for PCA ####

setMethod("plot",
	signature = c(x = "PCA", y = "missing"),
	function(x, formula = substitute(mode ~ mz),
		mode = "loadings",
		type = 'h',
		...)
	{
		mode <- match.arg(mode)
		callNextMethod(x, formula=formula, type=type, ...)
	})

setMethod("image",
	signature = c(x = "PCA"),
	function(x, formula = substitute(mode ~ x * y),
		mode = "scores",
		...)
	{
		mode <- match.arg(mode)
		callNextMethod(x, formula=formula, ...)
	})

#### Plotting for PLS ####

setMethod("plot",
	signature = c(x = "PLS", y = "missing"),
	function(x, formula = substitute(mode ~ mz),
		mode = c("coefficients", "loadings",
			"weights", "projection"),
		type = 'h',
		...)
	{
		mode <- match.arg(mode)
		callNextMethod(x, formula=formula, type=type, ...)
	})

setMethod("image",
	signature = c(x = "PLS"),
	function(x, formula = substitute(mode ~ x * y),
		mode = c("fitted", "scores", "y"),
		...)
	{
		mode <- match.arg(mode)
		callNextMethod(x, formula=formula, ...)
	})

#### Plotting for OPLS ####

setMethod("plot",
	signature = c(x = "OPLS", y = "missing"),
	function(x, formula = substitute(mode ~ mz),
		mode = c("coefficients", "loadings", "Oloadings",
			"weights", "Oweights", "projection"),
		type = 'h',
		...)
	{
		mode <- match.arg(mode)
		callNextMethod(x, formula=formula, type=type, ...)
	})

setMethod("image",
	signature = c(x = "OPLS"),
	function(x, formula = substitute(mode ~ x * y),
		mode = c("fitted", "scores", "Oscores", "y"),
		...)
	{
		mode <- match.arg(mode)
		callNextMethod(x, formula=formula, ...)
	})

#### Plotting for SpatialShrunkenCentroids ####

setMethod("plot",
	signature = c(x = "SpatialShrunkenCentroids", y = "missing"),
	function(x, formula = substitute(mode ~ mz),
		mode = c("centers", "tstatistics"),
		type = 'h',
		...)
	{
		mode <- match.arg(mode)
		callNextMethod(x, formula=formula, type=type, ...)
	})

setMethod("image",
	signature = c(x = "SpatialShrunkenCentroids"),
	function(x, formula = substitute(mode ~ x * y),
		mode = c("probabilities", "classes", "scores"),
		...)
	{
		mode <- match.arg(mode)
		callNextMethod(x, formula=formula, ...)
	})

#### Plotting for SpatialKMeans ####

setMethod("plot",
	signature = c(x = "SpatialKMeans", y = "missing"),
	function(x, formula = substitute(mode ~ mz),
		mode = c("centers", "betweenss", "withinss"),
		type = 'h',
		...)
	{
		mode <- match.arg(mode)
		callNextMethod(x, formula=formula, type=type, ...)
	})

setMethod("image",
	signature = c(x = "SpatialKMeans"),
	function(x, formula = substitute(mode ~ x * y),
		mode = "cluster",
		...)
	{
		mode <- match.arg(mode)
		callNextMethod(x, formula=formula, ...)
	})
