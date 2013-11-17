
### implement methods for cross-validation ####

setMethod("crossValidate", c("MSImageSet"), function(x, y,
	doMethod=c("spatialClassify", "spatialSparseClassify", "PLS", "OPLS"),
	nfolds, foldid=NULL, ...)
{
	doMethod <- match.fun(doMethod)
	if ( is.null(foldid) ) foldid <- sample(nfold, sum(!is.na(y)), replace=TRUE)
	if ( missing(nfolds) ) nfolds <- length(unique(foldid))
	results <- list()
	for ( i in seq_len(nfolds) ) {
		tryVerboseMessage("Fold ", i)
		y.train <- y
		y.train[foldid == i] <- NA
		fit <- doMethod(x, y.train, ...)
		y.test <- y
		y.test[foldid != i] <- NA
		results[[i]] <- predict(fit, x, y.test, ...)
	}
	names(results) <- paste("Fold", 1:nfolds)
	class(results) <- "CrossValidation"
	return(results)
} )

