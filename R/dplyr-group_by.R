
## Group a DataFrame

group_by.DataFrame <- function(.data, ..., add = FALSE, .drop = FALSE)
	{
		.Deprecated()
		group_by(as_tibble(as.list(.data)), ..., add=add, .drop=.drop)
	}

group_by.XDataFrame <- function(.data, ..., add = FALSE, .drop = FALSE)
	{
		.Deprecated()
		group_by(as_tibble(as.list(.data)), ..., add=add, .drop=.drop)
	}

groups.XDataFrame <- function(x)
	{
		.Deprecated()
		NULL
	}

ungroup.XDataFrame <- function(x, ...)
	{
		.Deprecated()
		x
	}

