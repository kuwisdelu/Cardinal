
setMethod("image", c(x = "SparseImagingSummary"),
	function(x, formula, feature, feature.groups, ...)
{
	.checkForIncompleteProcessing(x)
	if ( missing(formula) ) {
		values <- names(imageData(x))[1L]
		formula <- .formula_pixel_results(x, values)
		if ( !is.null(featureData(x)[["summary"]]) )
			formula <- paste0(c(deparse(formula), "summary"), collapse="|")
	}
	if ( missing(feature) )
		feature <- features(x)
	if ( missing(feature.groups) ) {
		if ( !is.null(featureData(x)[["group"]]) ) {
			feature.groups <- featureData(x)[["group"]]
		} else {
			feature.groups <- NULL
		}
	}
	callNextMethod(x, formula=as.formula(formula),
		feature=feature, feature.groups=feature.groups, ...)
})


setMethod("image", c(x = "MSImagingSummary"),
	function(x, ...)
{
	image(as(x, "SparseImagingSummary"), ...)
})


