
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
			normalize.do(s, object, .Index, fun, plot, ...)
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

normalize.do <- function(s, object, pixel, f, plot, ...) {
	sout <- f(s, ...)
	if ( plot ) {
		wrap(plot(object, s ~ mz, pixel=pixel, col="gray",
			ylab="Intensity", strip=FALSE, ...),
			..., signature=f)
		wrap(lines(mz(object), sout, lwd=0.5, ...),
			..., signature=f)
	}
	sout
}

normalize.method <- function(method, name.only=FALSE) {
	if ( is.character(method) || is.null(method) ) {
		options <- "tic"
		method <- match.method(method, options)
		if ( name.only )
			return(method)
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

