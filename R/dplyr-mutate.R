
## Add columns to a DataFrame

setMethod("mutate", "DataFrame",
	function(.data, ...)
	{
		mutate(as(.data, "XDataFrame"), ...)
	})

setMethod("mutate", "XDataFrame",
	function(.data, ...)
	{
		x <- mutate(.XDataFrame_to_tbl(.data), ...)
		x <- as(x, class(.data))
		groups(x) <- groups(.data)
		x
	})

setMethod("mutate", "SummaryDataFrame",
	function(.data, ...)
	{
		mutate(as.data.frame(.data), ...)
	})

## Add metadata columns to an imaging experiment

setMethod("mutate", "ImagingExperiment",
	function(.data, ...)
	{
		mdata <- mcols(.data)
		expr <- eval(substitute(alist(...)))
		nm <- sapply(substitute(...()), deparse)
		if ( !is.null(names(expr)) ) {
			nz <- nzchar(names(expr))
			nm[nz] <- names(expr)[nz]
		}
		names(expr) <- nm
		if ( length(expr) > 0 ) {
			e <- as.env(mdata, enclos=parent.frame(2))
			for ( i in seq_along(expr) ) {
				col <- eval(expr[[i]], envir=e)
				col <- rep_len(col, nrow(mdata))
				if ( nm[i] %in% ls(e) )
					rm(list=nm[i], pos=e)
				assign(nm[i], col, e)
				mdata[[nm[i]]] <- col
			}
			mcols(.data) <- mdata
		}
		.data
	})

