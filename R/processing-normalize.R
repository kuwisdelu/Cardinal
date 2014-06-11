
#### Normalization methods ####
## ----------------------------

setMethod("normalize", "MSImageSet",
	function(object, method = "tic",
		...,
		pixel=pixels(object),
		plot=FALSE)
	{
		fun <- normalize.method(method)
		data <- pixelApply(object, function(s, ...) {
			sout <- fun(s, ...)
			if ( plot ) {
				wrap(plot(object, pixel=.Index, col="gray", ...),
					..., signature=fun)
				wrap(lines(mz(object), sout, lwd=0.5, ...),
					..., signature=fun)
			}
			sout
		}, .pixel=pixel, ..., .use.names=FALSE, .simplify=TRUE)
		object@imageData <- SImageData(data=data,
			coord=coord(object)[pixel,],
			storageMode=storageMode(object@imageData),
			dimnames=list(featureNames(object), pixelNames(object)[pixel]))
		object@pixelData <- object@pixelData[pixel,]
		normalization(processingData(object)) <- match.method(method)
		prochistory(processingData(object)) <- .history()
		object
	})

normalize.method <- function(method) {
	if ( is.character(method) ) {
		method <- switch(method[[1]],
			tic = normalize.tic,
			match.fun(method))
	}
	match.fun(method)
}

normalize.tic <- function(x, tic=length(x), ...) {
	auc <- sum(x)
	if ( auc > 0 ) {
		tic * x / auc
	} else {
		rep(0, length(x))
	}
}

