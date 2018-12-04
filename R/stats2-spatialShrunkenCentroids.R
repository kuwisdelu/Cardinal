
setAs("SpatialShrunkenCentroids", "SpatialShrunkenCentroids2",
	function(from) {
		to <- .coerce_ResultImagingExperiment(from, "SpatialShrunkenCentroids2")
		metadata(to)$resultType <- list(feature=c("centers", "tstatistics"),
			pixel=c("probabilities", "classes", "scores"))
		to
	})


