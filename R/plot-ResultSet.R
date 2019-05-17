
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
		if ( !setequal(which, pixels(data)) )
			data <- data[,which]
		if ( missing(column) ) {
			column <- list(column=unique(df$column))
		} else if ( is.numeric(column) ) {
			column <- list(column=levels(df$column)[column])
		} else {
			column <- list(column=as.factor(column))
		}
		if ( missing(xlab) )
			xlab <- .format.plot.label(names(fm$right))
		if ( missing(ylab) )
			ylab <- .format.plot.label(names(fm$left))
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

#### Plotting for CrossValidated ####

setMethod("plot",
	signature = c(x = "CrossValidated", y = "missing"),
	function(x, fold = 1:length(x), layout, ...)
	{
		if ( !missing(layout) ) .setup.layout(rev(layout))
		for ( i in fold ) plot(resultData(x)[[i]], ...)
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

#### Plotting for SpatialFastmap ####

setMethod("plot",
	signature = c(x = "SpatialFastmap", y = "missing"),
	function(x, formula = substitute(mode ~ mz),
		mode = "correlation",
		type = 'h',
		...)
	{
		mode <- match.arg(mode)
		callNextMethod(x, formula=formula, type=type, ...)
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
