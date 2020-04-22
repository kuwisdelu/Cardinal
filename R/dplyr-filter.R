
## Subset a DataFrame by rows

filter.DataFrame <- function(.data, ..., .preserve = FALSE)
	{
		.Deprecated("subset")
		filter(as_tibble(as.list(.data)), ..., .preserve=.preserve)
	}

filter.XDataFrame <- function(.data, ..., .preserve = FALSE)
	{
		.Deprecated("subset")
		filter(as_tibble(as.list(.data)), ..., .preserve=.preserve)
	}

filter.SummaryDataFrame <- function(.data, ..., .preserve = FALSE)
	{
		.Deprecated("subset")
		filter(as.data.frame(.data), ..., .preserve=.preserve)
	}

## Subset an imaging dataset by rows/features

filter.SparseImagingExperiment <- function(.data, ..., .preserve = FALSE)
	{
		.Deprecated("subsetFeatures")
		.data[features(.data, ..., .env=parent.frame(1)),]
	}

