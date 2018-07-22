
#### Delayed/batched pre-processing ####
## -------------------------------------

setMethod("process", "SparseImagingExperiment",
	function(object, fun, ...,
			kind = c("pixel", "feature"),
			prefun, preargs,
			postfun, postargs,
			plotfun,
			label = "",
			delay = FALSE,
			plot = FALSE,
			par = NULL,
			layout,
			subset = TRUE,
			chunks = 0L,
			chunksize = ncol(object) / chunks,
			cache.chunks = FALSE,
			BPPARAM = bpparam())
	{
		if ( !missing(fun) ) {
			# get fun
			if ( is.null(fun) ) {
				kind <- "global"
				fun <- NULL
			} else {
				kind <- match.arg(kind)
				fun <- match.fun(fun)
			}
			# get preproc
			if ( missing(prefun) ) {
				prefun <- NULL
				has_pre <- FALSE
			} else {
				prefun <- match.fun(prefun)
				has_pre <- TRUE
			}
			if ( missing(preargs) )
				preargs <- NULL
			# get postproc
			if ( missing(postfun) ) {
				postfun <- NULL
				has_post <- FALSE
			} else {
				postfun <- match.fun(postfun)
				has_post <- TRUE
			}
			if ( missing(postargs) )
				postargs <- NULL
			# get plotfun
			if ( missing(plotfun) ) {
				plotfun <- NULL
				has_plot <- FALSE
			} else {
				plotfun <- match.fun(plotfun)
				has_plot <- TRUE
			}
			# create processing list
			proclist <- list(
				fun=fun, args=list(...),
				prefun=prefun, preargs=preargs,
				postfun=postfun, postargs=postargs,
				plotfun=plotfun)
			# create processing info
			procinfo <- DataFrame(
				label=label, kind=kind,
				pending=TRUE, complete=FALSE,
				has_pre=has_pre, has_post=has_post,
				has_plot=has_plot)
			# update object
			i <- length(processingData(object)) + 1L
			if ( label %in% names(processingData(object)) ) {
				processingData(object)[[i]] <- proclist
			} else {
				processingData(object)[[label]] <- proclist
			}
			if ( is.null(mcols(processingData(object))) ) {
				mcols(processingData(object)) <- procinfo
			} else {
				mcols(processingData(object))[i,] <- procinfo
			}
		}
		if ( !delay ) {
			if ( !missing(subset) ) {
				subset <- eval(substitute(subset),
					envir=as.env(pixelData(object),
						enclos=parent.frame(2)))
				if ( is.logical(subset) )
					subset <- rep_len(subset, ncol(object))
				object <- object[,subset]
			}
			if ( plot && !is(BPPARAM, "SerialParam") ) {
				.warning("plot=TRUE only allowed for SerialParam()")
				plot <- FALSE
				par <- NULL
			} else if ( plot && !missing(layout) ) {
				.setup.layout(layout)
			}
			object <- .delayedBatchProcess(object, plot=plot, par=par,
				.chunks=chunks, .chunksize=chunksize,
				.cache.chunks=cache.chunks,
				BPPARAM=BPPARAM)
		}
		if ( validObject(object) )
			object
	})

.delayedBatchProcess <- function(object, plot, par, ...) {
	queue <- .pendingQueue(processingData(object))
	if ( is.null(queue) )
		.warning("no pending processing steps to apply")
	while ( !is.null(queue) ) {
		proclist <- queue$queue
		# perform preprocessing
		if ( any(queue$info$has_pre) ) {
			if ( getOption("Cardinal.verbose") )
				.message("preprocessing ", queue$info$label[1L], " ...")
			prefun <- proclist[[1L]]$prefun
			preargs <- proclist[[1L]]$preargs
			prearglist <- c(list(object), preargs)
			object <- do.call(prefun, prearglist)
		}
		# apply processing to all pixels/features
		procfun <- function(.x, .list, .plot, .par, ...) {
			for ( i in seq_along(.list) ) {
				has_plotfun <- !is.null(.list[[i]]$plotfun)
				if ( .plot && has_plotfun )
					.xold <- .x
				fun <- .list[[i]]$fun
				arglist <- c(list(.x), .list[[i]]$args)
				.x <- do.call(fun, arglist)
				if ( .plot && has_plotfun ) {
					plotfun <- .list[[i]]$plotfun
					plotarglist <- c(list(.x), list(.xold), .par)
					do.call(plotfun, plotarglist)
				}
			}
			.x
		}
		by_pixels <- "pixel" %in% queue$info$kind
		by_features <- "feature" %in% queue$info$kind
		if ( getOption("Cardinal.verbose") && (by_pixels || by_features) ) {
			labels <- paste0(queue$info$label, collapse=" ")
			.message("processing ", labels, " ...")
		}
		if ( by_pixels ) {
			ans <- pixelApply(object, procfun,
				.list=proclist, .plot=plot, .par=par, ...,
				.simplify=FALSE)
		} else if ( by_features ) {
			ans <- featureApply(object, procfun,
				.list=proclist, .plot=plot, .par=par, ...,
				.simplify=FALSE)
		} else {
			ans <- NULL
		}
		# perform postprocessing
		if ( any(queue$info$has_post) ) {
			last <- length(proclist)
			if ( getOption("Cardinal.verbose") )
				.message("postprocessing ", queue$info$label[last], " ...")
			postfun <- proclist[[last]]$postfun
			postargs <- proclist[[last]]$postargs
			postarglist <- c(list(ans), list(object), postargs)
			object <- do.call(postfun, postarglist)
		} else {
			if ( by_pixels ) {
				iData(object) <- as.matrix(simplify2array(ans))
			} else if ( by_features ) {
				iData(object) <- t(simplify2array(ans))
			}
		}
		mcols(processingData(object))$pending[queue$index] <- FALSE
		mcols(processingData(object))$complete[queue$index] <- TRUE
		queue <- .pendingQueue(processingData(object))
	}
	object
}

.pendingQueue <- function(y) {
	x <- y[mcols(y)$pending]
	if ( length(x) == 0L )
		return(NULL)
	if ( mcols(x)$kind[1L] == "global" ) {
		index <- which(mcols(y)$pending)[1L]
	} else {
		kind_ok <- mcols(x)$kind == mcols(x)$kind[1L]
		pre_ok <- !mcols(x)$has_pre
		pre_ok[1L] <- TRUE
		post_ok <- !mcols(x)$has_post
		post_ok <- c(TRUE, post_ok[-length(post_ok)])
		ok <- kind_ok & pre_ok & post_ok
		index <- which(mcols(y)$pending)[ok]
	}
	list(index=index, info=mcols(y)[index,], queue=y[index])
}

.checkForIncompleteProcessing <- function(object) {
	if ( any(mcols(processingData(object))$pending) )
		.warning("object has incomplete processing steps; ",
			"run process() on it to apply them")
}

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
