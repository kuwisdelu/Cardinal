
setMethod("plot", c(x = "SparseImagingResult", y = "formula"),
	function(x, y, ...) plot(x, formula = y, ...))

setMethod("plot", c(x = "SparseImagingResult", y = "missing"),
	function(x, formula,
		model = modelData(x),
		superpose = is_matrix,
	    ...,
	    column,
		xlab, ylab,
		type = 'h')
{
	.checkForIncompleteProcessing(x)
	args <- .parseFormula2(formula)
	if ( length(args$lhs) != 1L )
		.stop("lhs of formula must include exactly 1 variable")
	if ( length(args$rhs) != 1L )
		.stop("rhs of formula must include exactly 1 variables")
	if ( !is.null(args$g) )
		.stop("conditioning variables via | not allowed")
	is_matrix <- !is.numeric_vector(resultData(x, 1, names(args$lhs)))
	newx <- .reshape_feature_results(x, names(args$lhs))
	formula2 <- paste0(c(deparse(formula), "model"), collapse="|")
	formula2 <- as.formula(formula2)
	if ( is.null(names(model)) ) {
		pixel1 <- subset_rows(coord(newx), list(model_id=model))
	} else {
		model <- model[names(model) %in% names(pData(newx))]
		pixel1 <- subset_rows(pData(newx), as.list(model))
	}
	ncols <- sort(unique(pData(newx)[["column"]]))
	nc <- length(ncols)
	if ( missing(column) )
		column <- ncols
	if ( is.numeric(column) ) {
		pixel2 <- subset_rows(coord(newx), list(column_id=column))
	} else {
		pixel2 <- subset_rows(pData(newx), list(column=column))
	}
	pixel <- intersect(pixel1, pixel2)
	if ( is_matrix ) {
		pixel.groups <- pixelData(newx)[["column"]][pixel]
	} else {
		pixel.groups <- NULL
	}
	if ( missing(xlab) )
		xlab <- names(args$rhs)
	if ( missing(ylab) )
		ylab <- names(args$lhs)
	plot(newx, formula=formula2, pixel=pixel,
		pixel.groups=pixel.groups, superpose=superpose,
		xlab=xlab, ylab=ylab, type=type, ...)
})

# format feature data

.reshape_feature_results <- function(object, name) {
	data <- lapply(resultData(object), function(res) {
		res <- res[[name, exact=FALSE]]
		if ( is.factor(res) || is.character(res) ) {
			.factor_matrix(res)
		} else if ( is.vector(res) ) {
			as.matrix(res)
		} else {
			res
		}
	})
	cols <- lapply(data, function(x) {
		if ( is.null(colnames(x))) {
			paste0(seq_len(ncol(x)))
		} else {
			colnames(x)
		}
	})
	pnm <- names(modelData(object))
	pdata <- lapply(seq_along(cols), function(i) {
		par <- as.list(modelData(object)[i,pnm,drop=FALSE])
		par[["model"]] <- .format.data.labels(par)
		par[["model"]] <- factor(par$model, levels=unique(par$model))
		par[["column"]] <- cols[[i]]
		len <- length(cols[[i]])
		coord <- DataFrame(column_id=seq_len(len), model_id=i)
		PositionDataFrame(run=factor(1), coord=coord, par)
	})
	pdata <- do.call("rbind", pdata)
	data <- SimpleList(do.call("cbind", data))
	names(data) <- name
	SparseImagingExperiment(imageData=data,
		featureData=featureData(object),
		pixelData=pdata)
}

# format formula

.formula_feature_results <- function(x, lhs) {
	rhs <- setdiff(ls(as.env(featureData(x))), names(featureData(x)))
	if ( length(rhs) > 0L ) {
		rhs <- rhs[1L]
	} else {
		rhs <- names(featureData(x))[1L]
	}
	fm <- paste0(lhs, "~", rhs)
	formula <- as.formula(fm, env=parent.frame(3))
}


## PCA2

setMethod("plot",
	signature = c(x = "PCA2", y = "missing"),
	function(x, formula, values = "loadings", ...)
	{
		if ( missing(formula) )
			formula <- .formula_feature_results(x, match.arg(values))
		callNextMethod(x, formula=formula, ...)
	})

## PLS2

setMethod("plot",
	signature = c(x = "PLS2", y = "missing"),
	function(x, formula, values = c("coefficients", "loadings", "weights"), ...)
	{
		if ( missing(formula) )
			formula <- .formula_feature_results(x, match.arg(values))
		callNextMethod(x, formula=formula, ...)
	})

## SpatialFastmap2

setMethod("plot",
	signature = c(x = "SpatialFastmap2", y = "missing"),
	function(x, formula, values = "correlation", ...)
	{
		if ( missing(formula) )
			formula <- .formula_feature_results(x, match.arg(values))
		callNextMethod(x, formula=formula, ...)
	})

## SpatialKMeans2

setMethod("plot",
	signature = c(x = "SpatialKMeans2", y = "missing"),
	function(x, formula, values = c("centers", "correlation"), ...)
	{
		if ( missing(formula) )
			formula <- .formula_feature_results(x, match.arg(values))
		callNextMethod(x, formula=formula, ...)
	})

## SpatialShrunkenCentroids2

setMethod("plot",
	signature = c(x = "SpatialShrunkenCentroids2", y = "missing"),
	function(x, formula, values = c("centers", "statistic", "sd"), ...)
	{
		if ( missing(formula) )
			formula <- .formula_feature_results(x, match.arg(values))
		callNextMethod(x, formula=formula, ...)
	})

## SpatialDGMM

setMethod("plot",
	signature = c(x = "SpatialDGMM", y = "missing"),
	function(x, model = modelData(x), values = "density", type = 'l', ...)
	{
		values <- match.arg(values)
		if ( is.null(names(model)) ) {
			i <- model
		} else {
			i <- subset_rows(modelData(x), as.list(model))
		}
		data <- lapply(i, function(ii) {
			e <- resultData(x, ii, "estimates")
			p <- plot_density(mean=e$mean, var=e$var,
				cnames=e$class, plot=FALSE)
			m <- modelData(x)[ii,,drop=FALSE]
			p$model <- .format.data.labels(m)
			p
		})
		data <- do.call("rbind", data)
		plot(data, density ~ x | model, groups=class, type=type, ...)
	})

## MeansTest

setMethod("plot",
	signature = c(x = "MeansTest", y = "missing"),
	function(x, model = modelData(x), values = "fixed", ...)
	{
		values <- match.arg(values)
		if ( is.null(names(model)) ) {
			i <- model
		} else {
			i <- subset_rows(modelData(x), as.list(model))
		}
		data <- lapply(i, function(ii) {
			d <- resultData(x, ii, "data")
			m <- modelData(x)[ii,,drop=FALSE]
			d$model <- .format.data.labels(m)
			d
		})
		data <- do.call("rbind", data)
		data$model <- factor(data$model, levels=unique(data$model))
		fm <- deparse(metadata(x)[[values]])
		fm <- paste0(c(fm, "model"), collapse="|")
		plot(data, formula=as.formula(fm), ...)
	})

## SegmentationTest

setMethod("plot",
	signature = c(x = "SegmentationTest", y = "missing"),
	function(x, model = modelData(x), values = "fixed", ...)
	{
		values <- match.arg(values)
		if ( is.null(names(model)) ) {
			i <- model
		} else {
			i <- subset_rows(modelData(x), as.list(model))
		}
		data <- lapply(i, function(ii) {
			d <- resultData(x, ii, "data")
			m <- modelData(x)[ii,,drop=FALSE]
			d$model <- .format.data.labels(m)
			d
		})
		data <- do.call("rbind", data)
		data$model <- factor(data$model, levels=unique(data$model))
		fm <- deparse(metadata(x)[[values]])
		fm <- paste0(c(fm, "model"), collapse="|")
		plot(data, formula=as.formula(fm), ...)
	})



