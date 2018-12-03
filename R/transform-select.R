
## Subset an imaging dataset by columns/pixels

setMethod("select", "ImagingExperiment",
	function(.data, ..., .id)
	{
		idx <- pixels(.data, ..., .env=parent.frame(2))
		if ( !missing(.id) )
			idx <- intersect(idx, .id)
		.data[,idx]
	})
