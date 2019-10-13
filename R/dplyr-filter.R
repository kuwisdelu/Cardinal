
## Subset a DataFrame by rows

filter.DataFrame <- function(.data, ..., .preserve = FALSE)
	{
		filter(as(.data, "XDataFrame"), ..., .preserve=.preserve)
	}

filter.XDataFrame <- function(.data, ..., .preserve = FALSE)
	{
		x <- filter(.XDataFrame_to_tbl(.data), ..., .preserve=.preserve)
		x <- as(x, class(.data))
		x@groups <- groups(.data)
		x
	}

filter.SummaryDataFrame <- function(.data, ..., .preserve = FALSE)
	{
		filter(as.data.frame(.data), ..., .preserve=.preserve)
	}

## Subset an imaging dataset by rows/features

filter.SparseImagingExperiment <- function(.data, ..., .preserve = FALSE)
	{
		.data[features(.data, ..., .env=parent.frame(1)),]
	}

