
#### Sample standardize methods ####
## ---------------------------------

setMethod("standardizeSamples", "MSImageSet",
	function(object, method = "sum", ...)
	{
		fun <- standardizeSamples.method(method)
		prochistory(processingData(object)) <- .history()
		.message("standardizeSamples: Using method = ", match.method(method))
		.time.start()
		data <- featureApply(object, function(s, ...) {
			fun(s, ...)
		}, .pixel.groups=sample, ..., .use.names=FALSE, .simplify=FALSE)
		data <- matrix(unlist(data), nrow=nrow(object), ncol=ncol(object),
			byrow=TRUE)
		object@imageData <- MSImageData(data=data,
			coord=coord(object)[order(pData(object)[["sample"]]),],
			storageMode=storageMode(object@imageData),
			dimnames=list(featureNames(object), pixelNames(object)))
		object@pixelData <- object@pixelData[order(pData(object)[["sample"]]),]
		.message("standardizeSamples: Done.")
		.time.stop()
		object
	})

standardizeSamples.method <- function(method) {
	if ( is.character(method) ) {
		method <- match.method(method, c("sum"))
		method <- switch(method,
			sum = standardizeSamples.sum,
			match.fun(method))
	}
	match.fun(method)
}

standardizeSamples.sum <- function(x, sum=length(x), ...) {
	auc <- sum(x)
	if ( auc > 0 ) {
		sum * x / auc
	} else {
		rep(0, length(x))
	}
}
