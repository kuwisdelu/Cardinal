
## Subset an imaging dataset by columns/pixels

setMethod("select", "ImagingExperiment",
	function(.data, ...)
	{
		.data[,pixels(.data, ..., .env=parent.frame(2))]
	})
