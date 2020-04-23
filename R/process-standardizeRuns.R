
#### Sample standardize methods ####
## ---------------------------------

setMethod("standardizeRuns", "MSImageSet",
	function(object, method = "sum", ...)
	{
		.Deprecated_Cardinal1()
		fun <- standardizeRuns.method(method)
		.message("standardizeRuns: Using method = ", match.method(method))
		.time.start()
		data <- featureApply(object, .fun=fun, .pixel.groups=sample, ...,
			.use.names=FALSE, .simplify=FALSE)
		data <- matrix(unlist(data), nrow=nrow(object), ncol=ncol(object),
			byrow=TRUE)
		object@imageData <- MSImageData(data=data,
			coord=coord(object)[order(pData(object)[["sample"]]),],
			storageMode=storageMode(object@imageData),
			dimnames=list(featureNames(object), pixelNames(object)))
		object@pixelData <- object@pixelData[order(pData(object)[["sample"]]),]
		.message("standardizeRuns: Done.")
		.time.stop()
		object
	})

standardizeRuns.method <- function(method, name.only=FALSE) {
	if ( is.character(method) || is.null(method) ) {
		options <- "sum"
		method <- match.method(method, options)
		if ( name.only )
			return(method)
		method <- switch(method,
			sum = standardizeRuns.sum,
			match.fun(method))
	}
	match.fun(method)
}

standardizeRuns.sum <- function(x, sum=length(x), ...) {
	auc <- sum(x)
	if ( auc > 0 ) {
		sum * x / auc
	} else {
		rep(0, length(x))
	}
}

