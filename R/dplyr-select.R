
## Subset a DataFrame by columns

setMethod("select", "DataFrame",
	function(.data, ...)
	{
		select(as(.data, "XDataFrame"), ...)
	})

setMethod("select", "SummaryDataFrame",
	function(.data, ...)
	{
		select(as.data.frame(.data), ...)
	})

setMethod("select", "XDataFrame",
	function(.data, ...)
	{
		x <- select(.XDataFrame_to_tbl(.data), ...)
		x <- as(x, class(.data))
		groups(x) <- groups(.data)
		x
	})

## Subset an imaging dataset by columns/pixels

setMethod("select", "ImagingExperiment",
	function(.data, ...)
	{
		.data[,pixels(.data, ..., .env=parent.frame(2))]
	})

