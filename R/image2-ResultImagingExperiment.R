
setMethod("image", c(x = "SparseResultImagingExperiment"),
	function(x, formula,
		model = modelData(x),
		superpose = TRUE,
		column,
	    ...,
		colorscale = divergent.colors,
		colorkey = !is3d && !superpose,
		subset = TRUE)
{
	.checkForIncompleteProcessing(x)
	args <- .parseFormula2(formula)
	if ( length(args$lhs) != 1L )
		.stop("lhs of formula must include exactly 1 variable")
	if ( !is.null(args$g) )
		.stop("conditioning variables via | not allowed")
	is3d <- length(args$rhs) == 3L
	newx <- .format_pixel_results(x, names(args$lhs))
	formula2 <- paste0(c(deparse(formula), "model"), collapse="|")
	formula2 <- as.formula(formula2)
	if ( is.null(names(model)) ) {
		feature1 <- subset_rows(fData(newx), list(model_id=model))
	} else {
		feature1 <- subset_rows(featureData(newx), as.list(model))
	}
	if ( missing(column) )
		column <- sort(unique(fData(newx)[["column"]]))
	if ( is.numeric(column) ) {
		feature2 <- subset_rows(fData(newx), list(column_id=column))
	} else {
		feature2 <- subset_rows(featureData(newx), list(column=column))
	}
	feature <- intersect(feature1, feature2)
	feature.groups <- featureData(newx)[["column"]][feature]
	image(newx, formula=formula2, feature=feature,
		feature.groups=feature.groups, superpose=superpose,
		colorscale=colorscale, colorkey=colorkey,
		subset=subset, ...)
})

## SpatialFastmap2

setMethod("image",
	signature = c(x = "SpatialFastmap2"),
	function(x, formula, values = "scores", ...)
	{
		if ( missing(formula) )
			formula <- .formula_pixel_results(x, match.arg(values))
		callNextMethod(x, formula=formula, ...)
	})

## SpatialKMeans2

setMethod("image",
	signature = c(x = "SpatialKMeans2"),
	function(x, formula, values = "cluster", ...)
	{
		if ( missing(formula) )
			formula <- .formula_pixel_results(x, match.arg(values))
		callNextMethod(x, formula=formula, ...)
	})

## SpatialShrunkenCentroids2

setMethod("image",
	signature = c(x = "SpatialShrunkenCentroids2"),
	function(x, formula, values = c("probabilities", "classes", "scores"), ...)
	{
		if ( missing(formula) )
			formula <- .formula_pixel_results(x, match.arg(values))
		callNextMethod(x, formula=formula, ...)
	})

# format formula

.formula_pixel_results <- function(x, lhs) {
	fm <- paste0(lhs, "~", paste0(coordnames(x)[c(1,2)], collapse="*"))
	formula <- as.formula(fm, env=parent.frame(3))
}

# format pixel data

.format_pixel_results <- function(object, name) {
	data <- lapply(resultData(object), function(res) {
		res <- res[[name]]
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
	fdata <- lapply(seq_along(cols), function(i) {
		par <- as.list(modelData(object)[i,])
		par[["model"]] <- .format.data.labels(par)
		par[["column"]] <- cols[[i]]
		len <- length(cols[[i]])
		par1 <- DataFrame(column_id=seq_len(len), model_id=i)
		par2 <- DataFrame(par)		
		cbind(par1, par2)
	})
	fdata <- as(do.call("rbind", fdata), "XDataFrame")
	data <- SimpleList(t(do.call("cbind", data)))
	names(data) <- name
	SparseImagingExperiment(imageData=data,
		featureData=fdata,
		pixelData=pixelData(object))
}



