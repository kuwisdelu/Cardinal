
setMethod("plot", c(x = "SparseResultImagingExperiment", y = "formula"),
	function(x, y, ...) plot(x, formula = y, ...))

setMethod("plot", c(x = "SparseResultImagingExperiment", y = "missing"),
	function(x, formula,
		model = modelData(x),
		superpose = TRUE,
	    ...,
	    column,
		xlab, ylab,
		type = 'h',
		subset = TRUE)
{
	.checkForIncompleteProcessing(x)
	args <- .parseFormula2(formula)
	if ( length(args$lhs) != 1L )
		.stop("lhs of formula must include exactly 1 variable")
	if ( length(args$rhs) != 1L )
		.stop("rhs of formula must include exactly 1 variables")
	if ( !is.null(args$g) )
		.stop("conditioning variables via | not allowed")
	newx <- .format_feature_results(x, names(args$lhs))
	formula2 <- paste0(c(deparse(formula), "model"), collapse="|")
	formula2 <- as.formula(formula2)
	if ( is.null(names(model)) ) {
		pixel1 <- subset_rows(coord(newx), list(model_id=model))
	} else {
		model <- model[names(model) %in% names(pData(newx))]
		pixel1 <- subset_rows(pData(newx), as.list(model))
	}
	cols <- sort(unique(pData(newx)[["column"]]))
	nc <- length(cols)
	if ( missing(column) )
		column <- cols
	if ( is.numeric(column) ) {
		pixel2 <- subset_rows(coord(newx), list(column_id=column))
	} else {
		pixel2 <- subset_rows(pData(newx), list(column=column))
	}
	pixel <- intersect(pixel1, pixel2)
	pixel.groups <- pixelData(newx)[["column"]][pixel]
	if ( missing(xlab) )
		xlab <- names(args$rhs)
	if ( missing(ylab) )
		ylab <- names(args$lhs)
	plot(newx, formula=formula2, pixel=pixel, pixel.groups=pixel.groups,
		superpose=superpose, xlab=xlab, ylab=ylab, type=type, subset=subset, ...)
})

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
	function(x, formula, values = c("centers", "statistic"), ...)
	{
		if ( missing(formula) )
			formula <- .formula_feature_results(x, match.arg(values))
		callNextMethod(x, formula=formula, ...)
	})

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

# format feature data

.format_feature_results <- function(object, name) {
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



