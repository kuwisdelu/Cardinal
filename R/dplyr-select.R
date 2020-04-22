
## Subset a DataFrame by columns

select.DataFrame <- function(.data, ...)
	{
		.Deprecated("subset")
		select(as_tibble(as.list(.data)), ...)
	}

select.XDataFrame <- function(.data, ...)
	{
		.Deprecated("subset")
		select(as_tibble(as.list(.data)), ...)
	}

select.SummaryDataFrame <- function(.data, ...)
	{
		.Deprecated("subset")
		select(as.data.frame(.data), ...)
	}

## Subset an imaging dataset by columns/pixels

select.SparseImagingExperiment <- function(.data, ...)
	{
		.Deprecated("subsetPixels")
		.data[,pixels(.data, ..., .env=parent.frame(1))]
	}

