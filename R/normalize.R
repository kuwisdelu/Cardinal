
#### Normalization methods ####
## ----------------------------

setMethod("normalize", "MSImageSet",
	function(object, method = "tic", ..., .plot=FALSE) {
		fun <- .match.method(method)
		spectra(object) <- pixelApply(object, function(s) {
			s <- fun(s, ...)
			if ( .plot )
				plot(mz(object), s, type="l", xlab="m/z",
					ylab="Intensity", ...)
			s
		}, ..., .use.names=FALSE)
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
