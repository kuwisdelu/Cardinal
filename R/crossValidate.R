
#### Perform cross-validation ####
## -------------------------------

crossValidate <- function(fit., x, y, folds = run(x), ...,
	predict. = predict, keep.models = FALSE,
	trainProcess = peakProcess, trainArgs = list(),
	testProcess = peakProcess, testArgs = list(),
	nchunks = getCardinalNChunks(),
	verbose = getCardinalVerbose(),
	BPPARAM = getCardinalBPPARAM())
{
	cv_do(fit., x=x, y=y, folds=folds, transpose=TRUE,
		predict.=predict., keep.models=keep.models,
		trainProcess=trainProcess, trainArgs=trainArgs,
		testProcess=testProcess, testArgs=testArgs,
		nchunks=nchunks, verbose=verbose,
		BPPARAM=BPPARAM, ...)
}

