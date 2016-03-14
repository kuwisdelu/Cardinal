
#### Batch pre-processing ####
## ----------------------------

setMethod("batchProcess", "MSImageSet",
	function(object,
		normalize = NULL,
		smoothSignal = NULL,
		reduceBaseline = NULL,
		peakPick = NULL,
		...,
		layout,
		pixel = pixels(object),
		plot = FALSE)
	{
		if ( centroided(object) )
			.stop("Data already centroided. Processing will not be performed.")
		prochistory(processingData(object)) <- .history()
		.time.start()
		if ( missing(layout) )
			layout <- NULL
		if ( !is.null(layout) && plot )
			.setup.layout(layout)
		smean <- numeric(nrow(object))
		data <- pixelApply(object, function(s) {
			# handle feature-preserving pre-processing first
			normalize <<- batch.args(normalize)
			if ( !is.null(normalize) ) {
				fun <- normalize.method(normalize$method)
				args <- c(list(s, object, .Index, fun, plot), normalize)
				args$method <- NULL
				s <- do.call(normalize.do, args)
			}
			smoothSignal <<- batch.args(smoothSignal)
			if ( !is.null(smoothSignal) ) {
				fun <- smoothSignal.method(smoothSignal$method)
				args <- c(list(s, object, .Index, fun, plot), smoothSignal)
				args$method <- NULL
				s <- do.call(smoothSignal.do, args)
			}
			reduceBaseline <<- batch.args(reduceBaseline)
			if ( !is.null(reduceBaseline) ) {
				fun <- reduceBaseline.method(reduceBaseline$method)
				args <- c(list(s, object, .Index, fun, plot), reduceBaseline)
				args$method <- NULL
				s <- do.call(reduceBaseline.do, args)
			}
			# save the mean spectrum
			smean <<- s + smean
			# need to handle peak picking differently
			peakPick <<- batch.args(peakPick)
			if ( !is.null(peakPick) ) {
				fun <- peakPick.method(peakPick$method)
				args <- c(list(s, object, .Index, fun, plot), peakPick)
				args$method <- NULL
				p <- do.call(peakPick.do, args)
			}
			# return only the named peaks if they exist
			if ( exists("p", inherits=FALSE) ) {
				names(s) <- featureNames(object)
				s <- s[p]
			}
			s
		}, .pixel=pixel, .use.names=FALSE, .simplify=FALSE)
		smean <- smean / length(data)
		if ( is.null(peakPick) ) {
			# handle feature-preserving pre-processing
			data <- simplify2array(data)
			object@imageData <- MSImageData(data=data,
				coord=coord(object)[pixel,],
				storageMode=storageMode(object@imageData),
				dimnames=list(featureNames(object), pixelNames(object)[pixel]))
			object@pixelData <- object@pixelData[pixel,]
			object@featureData[["mean.intensity"]] <- smean
		} else if ( !is.null(peakPick) ) {
			# handle peak-picked data
			peakData <- data
			mzData <- lapply(data, function(p) {
				pmz <- mz(object)[match(names(p), featureNames(object))]
				names(pmz) <- names(p)
				pmz
			})
			object <- object[,pixel]
			data <- new("Hashmat", data=data, keys=featureNames(object),
				dim=c(length(features(object)), length(data)))
			peakData <- new("Hashmat", data=peakData, keys=featureNames(object),
				dim=c(length(features(object)), length(peakData)))
			mzData <- new("Hashmat", data=mzData, keys=featureNames(object),
				dim=c(length(features(object)), length(mzData)))
			iData(imageData(object)) <- data
			peakData(imageData(object)) <- peakData
			mzData(imageData(object)) <- mzData
			object@featureData[["mean.intensity"]] <- smean
		}
		if ( !is.null(normalize) )
			normalization(processingData(object)) <- normalize.method(
				normalize$method, name.only=TRUE)
		if ( !is.null(smoothSignal) )
			smoothing(processingData(object)) <- smoothSignal.method(
				smoothSignal$method, name.only=TRUE)
		if ( !is.null(reduceBaseline) )
			baselineReduction(processingData(object)) <- reduceBaseline.method(
				reduceBaseline$method, name.only=TRUE)
		if ( !is.null(peakPick) )
			peakPicking(processingData(object)) <- peakPick.method(
				peakPick$method, name.only=TRUE)
		.time.stop()
		object
	})

batch.args <- function(args) {
	if ( (is.logical(args) && !args) || is.null(args) ) {
		NULL
	} else if ( isTRUE(args) ) {
		list()
	} else {
		as.list(args)
	}
}
