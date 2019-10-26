
setMethod("plot", c(x = "SparseImagingSummary", y = "formula"),
	function(x, y, ...) plot(x, formula = y, ...))

setMethod("plot", c(x = "SparseImagingSummary", y = "missing"),
	function(x, formula, pixel, pixel.groups, ...)
{
	.checkForIncompleteProcessing(x)
	if ( missing(formula) ) {
		values <- names(imageData(x))[1L]
		formula <- .formula_feature_results(x, values)
		if ( !is.null(pixelData(x)[["summary"]]) )
			formula <- paste0(c(deparse(formula), "summary"), collapse="|")
	}
	if ( missing(pixel) )
		pixel <- pixels(x)
	if ( missing(pixel.groups) ) {
		if ( !is.null(pixelData(x)[["group"]]) ) {
			pixel.groups <- pixelData(x)[["group"]]
		} else {
			pixel.groups <- NULL
		}
	}
	callNextMethod(x, formula=as.formula(formula),
		pixel=pixel, pixel.groups=pixel.groups, ...)
})

setMethod("plot", c(x = "MSImagingSummary", y = "missing"),
	function(x, formula, ..., xlab, ylab,
		type = if ( is_centroided ) 'h' else 'l')
{
	is_centroided <- isTRUE(centroided(x))
	if ( missing(xlab) && missing(ylab) && missing(formula) ) {
		xlab <- expression(italic(m/z))
		ylab <- expression(italic(Intensity))
	}
	plot(as(x, "SparseImagingSummary"), formula=formula, ...,
		xlab=xlab, ylab=ylab, type=type)
})

