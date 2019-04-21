
## Subset a DataFrame by rows

setMethod("filter", "DataFrame",
	function(.data, ..., .preserve = FALSE)
	{
		filter(as(.data, "XDataFrame"), ..., .preserve=.preserve)
	})

setMethod("filter", "XDataFrame",
	function(.data, ..., .preserve = FALSE)
	{
		x <- filter(.XDataFrame_to_tbl(.data), ..., .preserve=.preserve)
		x <- as(x, class(.data))
		groups(x) <- groups(.data)
		x
	})

## Subset an imaging dataset by rows/features

setMethod("filter", "ImagingExperiment",
	function(.data, ..., .preserve = FALSE)
	{
		.data[features(.data, ..., .env=parent.frame(2)),]
	})

