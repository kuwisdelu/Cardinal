
## Subset an imaging dataset by rows/features

setMethod("filter", "ImagingExperiment",
	function(.data, ..., .preserve = FALSE)
	{
		.data[features(.data, ..., .env=parent.frame(2)),]
	})
