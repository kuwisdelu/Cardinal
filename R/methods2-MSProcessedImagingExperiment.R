
# 'processed' imaging experiments

setReplaceMethod("mz", "MSProcessedImagingExperiment",
	function(object, value) {
		if ( !all(mz(object@featureData) == keys(object@imageData)) )
			return(callNextMethod(object, value))
		if ( length(value) != length(mz(object)) ) {
			if ( ncol(featureData(object)) > 0L ) {
				drop <- names(featureData(object))
				.message("dropping feature metadata cols: ",
					paste0(drop, collapse=" "))
			}
			mcols <- MassDataFrame(mz=value)
			metadata(mcols) <- metadata(object@featureData)
			object@featureData <- mcols
		} else {
			mz(object@featureData) <- value
		}
		keys(object@imageData) <- value
		res <- attr(value, "resolution")
		tol <- attr(value, "tolerance")
		if ( !is.null(res) && is.null(tol) )
			tol <- switch(names(res),
				ppm = c(relative = res * 1e-6),
				mz = c(absolute = res / 2))
		if ( !is.null(tol) )
			tolerance(object@imageData) <- tol
		if ( validObject(object) )
			object
	})

setReplaceMethod("resolution", "MSProcessedImagingExperiment",
	function(object, value) {
		if ( !is.null(names(value)) ) {
			units <- switch(names(value), ppm="ppm", mz="mz")
		} else if ( !is.null(names(resolution(object))) ) {
			units <- switch(names(resolution(object)), ppm="ppm", mz="mz")
		}
		mz <- mz(from=min(mz(object)), to=max(mz(object)),
			resolution=value, units=units)
		mz(object) <- mz
		object
	})

setReplaceMethod("imageData", "MSProcessedImagingExperiment",
	function(y, value) {
		if ( !inherits(value, c("MSProcessedImagingSpectraList")) )
			y <- as(y, "MSImagingExperiment")
		callNextMethod(y, value=value)
	})

setReplaceMethod("iData", "MSProcessedImagingExperiment",
	function(x, i, ..., value) {
		if ( !inherits(value, "sparse_matc") ) {
			x <- as(x, "MSImagingExperiment")
			imageData(x) <- .SimpleImageArrayList(imageData(x))
		}
		callNextMethod(x, i, ..., value=value)
	})

setMethod("mzData", "MSProcessedImagingExperiment",
	function(object, ...) atomdata(iData(object))[["keys"]])

setReplaceMethod("mzData", "MSProcessedImagingExperiment",
	function(object, ..., value) {
		atomdata(iData(object))[["keys"]] <- value
		if ( validObject(object) )
			object
	})

setMethod("intensityData", "MSProcessedImagingExperiment",
	function(object, ...) atomdata(iData(object))[["values"]])

setReplaceMethod("intensityData", "MSProcessedImagingExperiment",
	function(object, ..., value) {
		atomdata(iData(object))[["values"]] <- value
		if ( validObject(object) )
			object
	})

setMethod("tolerance", "MSProcessedImagingExperiment",
	function(object) tolerance(imageData(object)))

setReplaceMethod("tolerance", "MSProcessedImagingExperiment",
	function(object, value) {
		tolerance(imageData(object)) <- value
		object
	})

setMethod("combiner", "MSProcessedImagingExperiment",
	function(object) combiner(imageData(object)))

setReplaceMethod("combiner", "MSProcessedImagingExperiment",
	function(object, value) {
		combiner(imageData(object)) <- value
		object
	})

collect.MSProcessedImagingExperiment <- function(x, ..., as.matrix = FALSE)
	{
		if ( as.matrix )
			return(NextMethod())
		fun <- function(y) {
			atomdata(y)[["keys"]] <- as.list(atomdata(y)[["keys"]])
			atomdata(y)[["values"]] <- as.list(atomdata(y)[["values"]])
			y
		}
		data <- as(imageData(x), "SimpleList", strict=FALSE)
		imageData(x) <- as(endoapply(data, fun), "MSProcessedImagingSpectraList")
		if ( validObject(x) )
			x
	}


