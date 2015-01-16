
#### Normalization methods ####
## ----------------------------

setMethod("normalize", "MSImageSet",
	function(object, method = "tic",
		...,
		pixel = pixels(object),
		plot = FALSE)
	{
		fun <- normalize.method(method)
		prochistory(processingData(object)) <- .history()
		.message("normalize: Using method = ", match.method(method))
		.time.start()
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
		object@imageData <- MSImageData(data=data,
			coord=coord(object)[pixel,],
			storageMode=storageMode(object@imageData),
			dimnames=list(featureNames(object), pixelNames(object)[pixel]))
		object@pixelData <- object@pixelData[pixel,]
		normalization(processingData(object)) <- match.method(method)
		.message("normalize: Done.")
		.time.stop()
		object
	})

normalize.method <- function(method) {
	if ( is.character(method) ) {
		method <- match.method(method, c("tic"))
		method <- switch(method,
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

