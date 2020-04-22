
## Arrange a DataFrame by row

arrange.DataFrame <- function(.data, ...)
	{
		.Deprecated("sort")
		arrange(as_tibble(as.list(.data)), ...)
	}

arrange.XDataFrame <- function(.data, ...)
	{
		arrange(as_tibble(as.list(.data)), ...)
	}

arrange.SummaryDataFrame <- function(.data, ...)
	{
		.Deprecated("sort")
		arrange(as.data.frame(.data), ...)
	}

