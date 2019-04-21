
## Arrange a DataFrame by row

setMethod("arrange", "DataFrame",
	function(.data, ...)
	{
		select(as(.data, "XDataFrame"), ...)
	})

setMethod("arrange", "XDataFrame",
	function(.data, ...)
	{
		x <- arrange(.XDataFrame_to_tbl(.data), ...)
		x <- as(x, class(.data))
		groups(x) <- groups(.data)
		x
	})

