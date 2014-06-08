
#### Normalization methods ####
## ----------------------------

setMethod("normalize", "MSImageSet",
	function(object, method = "tic",
		...,
		pixel=pixels(object),
		plot=TRUE)
	{
		fun <- match.method(method)
		data <- pixelApply(object, function(s) {
			s <- fun(s, ...)
			if ( plot )
				wrap(plot(mz(object), s, type="l", xlab="m/z", ylab="Intensity", ...),
					..., signature=fun)
			s
		}, .pixel=pixel, ..., .use.names=FALSE)
		object@imageData <- SImageData(data=data,
			coord=coord(object)[pixel,],
			storageMode=storageMode(object@imageData),
			dimnames=list(featureNames(object), pixelNames(object)[pixel]))
		object@pixelData <- object@pixelData[pixel,]
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

