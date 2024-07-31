
#### Perform cross-validation ####
## -------------------------------

crossValidate <- function(fit., x, y, folds = run(x), ...,
	predict. = predict, keep.models = FALSE,
	trainProcess = peakProcess, trainArgs = list(),
	testProcess = peakProcess, testArgs = list(),
	verbose = getCardinalVerbose(),
	BPPARAM = getCardinalBPPARAM())
{
	if ( !is.function(fit.) )
		.Defunct(msg="crossValidate() signature has changed, see ?crossValidate")
	ans <- cv_do(fit., x=x, y=y, folds=folds, transpose=TRUE,
		predict.=predict., keep.models=keep.models, mi=FALSE,
		trainProcess=trainProcess, trainArgs=trainArgs,
		testProcess=testProcess, testArgs=testArgs,
		verbose=verbose, BPPARAM=BPPARAM, ...)
	as(SpatialResults(ans, x), "SpatialCV")
}

setMethod("fitted", "SpatialCV",
	function(object, type = c("response", "class"), ...)
{
	type <- match.arg(type)
	fitted(object@model, type=type, ...)
})

setMethod("image", c(x = "SpatialCV"),
	function(x, i = 1L, type = c("response", "class"),
		layout = NULL, free = "", ...)
{
	type <- match.arg(type)
	y <- fitted(x, type=type, simplify=FALSE)
	FUN <- function(yi, ...) .plot_image_results(x, yi, ...)
	images <- lapply(y[i], FUN, ...)
	if ( is.null(names(images)) )
		names(images) <- paste0("i = ", i)
	if ( !is.null(layout) ) {
		layout <- rep_len(layout, 2L)
		nrow <- layout[1L]
		ncol <- layout[2L]
		as_facets(images, nrow=nrow, ncol=ncol, free=free)
	} else {
		as_facets(images, free=free)
	}
})
