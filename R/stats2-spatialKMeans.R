
setAs("SpatialKMeans", "SpatialKMeans2",
	function(from) {
		to <- .coerce_ResultImagingExperiment(from, "SpatialKMeans2")
		metadata(to)$resultType <- list(pixel="cluster",
			feature=c("centers", "betweenss", "withinss"))
		to
	})


