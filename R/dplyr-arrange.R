
## Arrange a DataFrame by row

arrange.DataFrame <- function(.data, ...)
	{
		arrange(as(.data, "XDataFrame"), ...)
	}

arrange.XDataFrame <- function(.data, ...)
	{
		x <- arrange(.XDataFrame_to_tbl(.data), ...)
		x <- as(x, class(.data))
		x@groups <- groups(.data)
		x
	}

arrange.SummaryDataFrame <- function(.data, ...)
	{
		arrange(as.data.frame(.data), ...)
	}

