
## Arrange a DataFrame by row

setMethod("arrange", "DataFrame",
	function(.data, ...)
	{
		arrange(as(.data, "XDataFrame"), ...)
	})

setMethod("arrange", "SummaryDataFrame",
	function(.data, ...)
	{
		arrange(as.data.frame(.data), ...)
	})

setMethod("arrange", "XDataFrame",
	function(.data, ...)
	{
		x <- arrange(.XDataFrame_to_tbl(.data), ...)
		x <- as(x, class(.data))
		groups(x) <- groups(.data)
		x
	})

