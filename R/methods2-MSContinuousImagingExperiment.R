
# 'continuous' imaging experiments

setReplaceMethod("imageData", "MSContinuousImagingExperiment",
	function(y, value) {
		if ( !inherits(value, c("MSContinuousImagingSpectraList")) ) {
			y <- as(y, "MSImagingExperiment")
			imageData(y) <- value
		} else {
			y <- callNextMethod(y, value)
		}
		y
	})

setReplaceMethod("iData", c("MSContinuousImagingExperiment", "ANY"),
	function(x, i, ..., value) {
		if ( !inherits(value, c("matrix", "matter_mat")) ) {
			x <- as(x, "MSImagingExperiment")
			imageData(x) <- .SimpleImageArrayList(imageData(x))
			iData(x, i, ...) <- value
		} else {
			x <- callNextMethod(x, i, ..., value=value)
		}
		x
	})

setReplaceMethod("iData", c("MSContinuousImagingExperiment", "missing"),
	function(x, i, ..., value) {
		if ( !inherits(value, c("matrix", "matter_mat")) ) {
			x <- as(x, "MSImagingExperiment")
			imageData(x) <- .SimpleImageArrayList(imageData(x))
			iData(x, ...) <- value
		} else {
			x <- callNextMethod(x, ..., value=value)
		}
		x
	})

