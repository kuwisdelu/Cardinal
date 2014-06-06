
#### Normalization methods ####
## ----------------------------

setMethod("normalize", "MSImageSet",
	function(object, method = "tic", ...) {
		fun <- .match.method(method)
		spectra(object) <- pixelApply(object, fun, ..., .use.names=FALSE)
		normalization(processingData(object)) <- method
		prochistory(processingData(object)) <- .history()
		object
	})

normalize.tic <- function(x, tic=length(x), ...) {
	auc <- sum(x)
	if ( auc > 0 ) {
		tic * x / auc
	} else {
		rep(0, length(x))
	}
}
