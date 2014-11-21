
#### Peak alignment methods ####
## ---------------------------

setMethod("peakAlign", signature = c(object = "MSImageSet", ref = "numeric"),
	function(object, ref, method = c("diff", "DP"),
		...,
		pixel = pixels(object),
		plot = FALSE)
	{
		if ( is.null(mzData(imageData(object))) )
			.stop("peakAlign: No peak picking has been applied. Nothing to align.")
		fun <- peakAlign.method(method)
		prochistory(processingData(object)) <- .history()
		.message("peakAlign: Using method = ", match.method(method))
		.time.start()
		peaklist <- pData(mzData(imageData(object)))
		alignment <- pixelApply(object, function(s, ...) {
			aout <- fun(peaklist[[.Index]], ref, ...)
			if ( plot ) {
				wrap(plot(object, pixel=.Index, lwd=2, ...),
					..., signature=fun)
				wrap(abline(v=ref, lty=2, lwd=0.5, col="blue", ...),
					..., signature=fun)
				wrap(abline(v=peaklist[[.Index]][!is.na(aout)], lty=3, lwd=1.5, col="red", ...),
					..., signature=fun)
			}
			aout
		}, .pixel=pixel, ..., .use.names=FALSE, .simplify=FALSE)
		alignment <- lapply(alignment, function(a) {
			a <- unlist(a)
			if ( length(a) == 0 ) {
				integer()
			} else {
				as.integer(a)
			}
		})
		feature <- features(object, mz=ref)
		object@featureData <- object@featureData[feature,]
		object@pixelData <- object@pixelData[pixel,]
		peakData <- new("Hashmat",
			data=pData(peakData(imageData(object)))[pixel],
			keys=featureNames(object@featureData),
			dim=c(nrow(object@featureData), nrow(object@pixelData)))
		mzData <- new("Hashmat",
			data=pData(mzData(imageData(object)))[pixel],
			keys=featureNames(object@featureData),
			dim=c(nrow(object@featureData), nrow(object@pixelData)))
		keys(peakData) <- alignment
		keys(mzData) <- alignment
		object@imageData <- MSImageData(data=peakData[,,drop=FALSE],
			coord=coord(object@pixelData),
			storageMode=storageMode(imageData(object)),
			dimnames=list(
				featureNames(object@featureData),
				pixelNames(object@pixelData)))
		peakData(imageData(object)) <- peakData
		mzData(imageData(object)) <- mzData
		mz(object) <- ref
		spectrumRepresentation(processingData(object)) <- "centroid"
		centroided(processingData(object)) <- TRUE
		.message("peakAlign: Done.")
		.time.stop()
		object
	})

setMethod("peakAlign", signature = c(object = "MSImageSet", ref = "MSImageSet"),
	function(object, ref, ...)
	{
		if ( is.null(mzData(imageData(object))) )
			.stop("peakAlign: No peak picking has been applied. Nothing to align.")
		prochistory(processingData(object)) <- .history()
		.message("peakAlign: Generating reference from mean mass spectrum.")
		spectrum <- featureApply(ref, mean)
		peaks <- mz(ref)[localMaximaLogical(spectrum, span=5)]
		peakAlign(object, ref=peaks, ...)
	})

setMethod("peakAlign", signature = c(object = "MSImageSet", ref = "missing"),
	function(object, ...) {
		prochistory(processingData(object)) <- .history()
		peakAlign(object, ref=object, ...)
	})

peakAlign.method <- function(method) {
	if ( is.character(method) ) {
		method <- match.method(method, c("diff", "DP"))
		method <- switch(method,
			diff = peakAlign.diff,
			DP = peakAlign.DP,
			match.fun(method))
	}
	match.fun(method)
}

peakAlign.diff <- function(x, y, diff.max=200, units=c("ppm", "mz"), ...) {
	if ( length(x) == 0 ) return(integer())
	units <- match.arg(units)
	if ( units == "ppm" ) {
		diff.max <- 1e-6 * diff.max * y
	} else if ( length(diff.max) != length(y) ) {
		diff.max <- rep(diff.max, length.out=length(y))
	}
	xmat <- matrix(x, nrow=length(y), ncol=length(x), byrow=TRUE)
	ymat <- matrix(y, nrow=length(y), ncol=length(x), byrow=FALSE)
	diffs <- abs(ymat - xmat)
	mins <- apply(diffs, 1, min)
	which <- apply(diffs, 1, which.min)
	aligned <- data.frame(x=which, y=seq_along(y))
	aligned <- aligned[mins <= diff.max,]
	matched <- rep(NA, length(x))
	matched[aligned$x] <- aligned$y
	matched
}

peakAlign.DP <- function(x, y, gap=0, ...) {
	if ( length(x) == 0 ) return(integer())
	aligned <- dynamicAlign(x, y, gap=gap, ...)
	matched <- rep(NA, length(x))
	matched[aligned[,"x"]] <- aligned[,"y"]
	matched
}

