
#### Batch pre-processing ####
## ----------------------------

setMethod("batchProcess", "MSImageSet",
	function(object,
		normalize = NULL,
		smoothSignal = NULL,
		reduceBaseline = NULL,
		reduceDimension = NULL,
		peakPick = NULL,
		peakAlign = NULL,
		...,
		layout,
		pixel = pixels(object),
		plot = FALSE)
	{
		if ( centroided(object) )
			.stop("batchProcess: Data already centroided. Processing will not be performed.")
		prochistory(processingData(object)) <- .history()
		normalize <- batch.args(normalize)
		smoothSignal <- batch.args(smoothSignal)
		reduceBaseline <- batch.args(reduceBaseline)
		reduceDimension <- batch.args(reduceDimension)
		peakPick <- batch.args(peakPick)
		peakAlign <- batch.args(peakAlign)
		if ( !is.null(reduceDimension) && !is.null(peakPick) )
			.stop("batchProcess: reduceDimension and peakPick cannot appear in the same call.")
		if ( !is.null(peakAlign) && is.null(peakPick) )
			.stop("batchProcess: peakAlign cannot appear without peakPick in the same call.")
		.time.start()
		if ( missing(layout) )
			layout <- NULL
		if ( !is.null(layout) && plot )
			.setup.layout(layout)
		out <- list()
		data <- pixelApply(object, function(s) {
			# handle feature-preserving pre-processing first
			if ( !is.null(normalize) ) {
				fun <- normalize.method(normalize$method)
				args <- c(list(s, object, .Index, fun, plot), normalize)
				args$method <- NULL
				s <- do.call(normalize.do, args)
			}
			if ( !is.null(smoothSignal) ) {
				fun <- smoothSignal.method(smoothSignal$method)
				args <- c(list(s, object, .Index, fun, plot), smoothSignal)
				args$method <- NULL
				s <- do.call(smoothSignal.do, args)
			}
			if ( !is.null(reduceBaseline) ) {
				fun <- reduceBaseline.method(reduceBaseline$method)
				args <- c(list(s, object, .Index, fun, plot), reduceBaseline)
				args$method <- NULL
				s <- do.call(reduceBaseline.do, args)
			}
			# need to handle reduce dimension differently
			if ( !is.null(reduceDimension) ) {
				fun <- reduceDimension.method(reduceDimension$method)
				args <- c(list(s, object, .Index, fun, plot), reduceDimension)
				args$method <- NULL
				s <- do.call(reduceDimension.do, args)
				if ( is.null(out$mz) )
					out$mz <<- s$t
				s <- s$x
			}
			# save the mean spectrum
			if ( is.null(out$mean) ) {
				out$mean <<- s
			} else {
				out$mean <<- s + out$mean
			}
			# need to handle peak picking differently
			if ( !is.null(peakPick) ) {
				fun <- peakPick.method(peakPick$method)
				args <- c(list(s, object, .Index, fun, plot), peakPick)
				args$method <- NULL
				p <- do.call(peakPick.do, args)
			}
			# return only the named peaks if they exist
			if ( exists("p", inherits=FALSE) ) {
				if ( is.null(out$mz) ) {
					names(s) <- featureNames(object)
				} else {
					names(s) <- .format.mz(out$mz)
				}
				s <- s[p]
			}
			s
		}, .pixel=pixel, .use.names=FALSE, .simplify=FALSE)
		# calculate the mean spectrum
		out$mean <- out$mean / length(data)
		if ( is.null(reduceDimension) && is.null(peakPick) ) {
			# handle feature-preserving pre-processing
			data <- simplify2array(data)
			object@imageData <- MSImageData(data=data,
				coord=coord(object)[pixel,],
				storageMode=storageMode(object@imageData),
				dimnames=list(featureNames(object), pixelNames(object)[pixel]))
			object@pixelData <- object@pixelData[pixel,]
			object@featureData[["mean"]] <- out$mean
		} else if ( !is.null(reduceDimension) ) {
			# handle reduce dimension
			data <- simplify2array(data)
			feature <- features(object, mz=out$mz)
			object@featureData <- object@featureData[feature,]
			object@pixelData <- object@pixelData[pixel,]
			object@imageData <- MSImageData(data=data,
				coord=coord(object@pixelData),
				storageMode=storageMode(imageData(object)),
				dimnames=list(
					featureNames(object@featureData),
					pixelNames(object@pixelData)))
			object@featureData[["mean"]] <- out$mean
			mz(object) <- out$mz
		} else if ( !is.null(peakPick) ) {
			# handle peak data
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
			object@featureData[["mean"]] <- out$mean
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
		if ( isTRUE(reduceDimension$method == "peaks") ) {
			spectrumRepresentation(processingData(object)) <- "centroid"
			centroided(processingData(object)) <- TRUE
		}
		if ( !is.null(peakPick) ) {
			.message("batchProcess: Processed spectra not saved. Peaks only will be returned.")
			if ( is.null(peakAlign) )
				.message("batchProcess: The returned spectra will be sparse.")
			peakPicking(processingData(object)) <- peakPick.method(
				peakPick$method, name.only=TRUE)
		}
		if ( !is.null(peakAlign) ) {
			# handle peak alignment (can't be integrated into earlier pixelApply)
			.message("batchProcess: Dispatching to peakAlign method.")
			args <- c(list(object=object, pixel=pixel, plot=plot), peakAlign)
			object <- do.call("peakAlign", args)
		}
		.message("batchProcess: Done.")
		.time.stop()
		object
	})

batch.args <- function(args, msg=TRUE) {
	if ( (is.logical(args) && !args) || is.null(args) ) {
		NULL
	} else {
		if ( msg )
			.message("batchProcess: ", deparse(substitute(args)), " = TRUE")
		if ( isTRUE(args) ) {
			list()
		} else {
			as.list(args)
		}
	}
}
