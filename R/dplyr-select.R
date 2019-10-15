
## Subset a DataFrame by columns

select.DataFrame <- function(.data, ...)
	{
		select(as(.data, "XDataFrame"), ...)
	}

select.XDataFrame <- function(.data, ...)
	{
		x <- select(.XDataFrame_to_tbl(.data), ...)
		x <- tryCatch(as(x, class(.data)),
			error=function(e) as(x, "XDataFrame"))
		x@groups <- groups(.data)
		x
	}

select.SummaryDataFrame <- function(.data, ...)
	{
		select(as.data.frame(.data), ...)
	}

## Subset an imaging dataset by columns/pixels

select.SparseImagingExperiment <- function(.data, ...)
	{
		.data[,pixels(.data, ..., .env=parent.frame(1))]
	}

