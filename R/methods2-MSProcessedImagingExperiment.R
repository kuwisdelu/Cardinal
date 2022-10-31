
# 'processed' imaging experiments

setReplaceMethod("mz", "MSProcessedImagingExperiment",
	function(object, value) {
		if ( !all(mz(object@featureData) == domain(object@imageData)) )
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
		domain(object@imageData) <- value
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
		if ( !inherits(value, c("MSProcessedImagingSpectraList")) ) {
			y <- as(y, "MSImagingExperiment")
			imageData(y) <- value
		} else {
			y <- callNextMethod(y, value)
		}
		y
	})

setReplaceMethod("iData", c("MSProcessedImagingExperiment", "ANY"),
	function(x, i, ..., value) {
		if ( !inherits(value, "sparse_mat") ) {
			x <- as(x, "MSImagingExperiment")
			imageData(x) <- .SimpleImageArrayList(imageData(x))
			iData(x, i, ...) <- value
		} else {
			x <- callNextMethod(x, i, ..., value=value)
		}
		x
	})

setReplaceMethod("iData", c("MSProcessedImagingExperiment", "missing"),
	function(x, i, ..., value) {
		if ( !inherits(value, "sparse_mat") ) {
			x <- as(x, "MSImagingExperiment")
			imageData(x) <- .SimpleImageArrayList(imageData(x))
			iData(x, ...) <- value
		} else {
			x <- callNextMethod(x, ..., value=value)
		}
		x
	})

setMethod("mzData", "MSProcessedImagingExperiment",
	function(object, ...) atomindex(iData(object)))

setReplaceMethod("mzData", "MSProcessedImagingExperiment",
	function(object, ..., value) {
		atomindex(iData(object)) <- value
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

setMethod("sampler", "MSProcessedImagingExperiment",
	function(object) sampler(imageData(object)))

setReplaceMethod("sampler", "MSProcessedImagingExperiment",
	function(object, value) {
		sampler(imageData(object)) <- value
		object
	})

