
# 'continuous' imaging experiments

setReplaceMethod("imageData", "MSContinuousImagingExperiment",
	function(y, value) {
		if ( !inherits(value, c("MSContinuousImagingSpectraList")) )
			y <- as(y, "MSImagingExperiment")
		callNextMethod(y, value=value)
	})

setReplaceMethod("iData", c("MSContinuousImagingExperiment", "missing"),
	function(x, i, ..., value) {
		if ( !inherits(value, c("matrix", "matter_matc")) ) {
			x <- as(x, "MSImagingExperiment")
			imageData(x) <- .SimpleImageArrayList(imageData(x))
		}
		callNextMethod(x, ..., value=value)
	})

setReplaceMethod("iData", c("MSContinuousImagingExperiment", "ANY"),
	function(x, i, ..., value) {
		if ( !inherits(value, c("matrix", "matter_matc")) ) {
			x <- as(x, "MSImagingExperiment")
			imageData(x) <- .SimpleImageArrayList(imageData(x))
		}
		callNextMethod(x, i=i, ..., value=value)
	})

