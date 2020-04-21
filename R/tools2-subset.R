
## Subset an imaging dataset with non-standard evaluation

setMethod("subset", "SparseImagingExperiment",
	function(x, subset, select, ...)
	{
		pdata <- as.env(pixelData(x), enclos=parent.frame(2))
		fdata <- as.env(featureData(x), enclos=parent.frame(2))
		if ( !missing(subset) ) {
			i <- eval(substitute(subset), envir=fdata)
			if ( !is.logical(i) && !all(is.wholenumber(i)) )
				.stop("'subset' must be logical or numeric indices")
		}
		if ( !missing(select) ) {
			j <- eval(substitute(select), envir=pdata)
			if ( !is.logical(j) && !all(is.wholenumber(j)) )
				.stop("'select' must be logical or numeric indices")
		}
		if ( !missing(subset) && !missing(select) ) {
			x[i,j]
		} else if ( !missing(subset) ) {
			x[i,]
		} else if ( !missing(select) ) {
			x[,j]
		} else {
			x
		}
	})

