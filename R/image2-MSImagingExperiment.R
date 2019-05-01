
#### Image plotting for MSImagingExperiment ####
## ------------------------------------------------

setMethod("image",
	signature = c(x = "MSImagingExperiment"),
	function(x, formula,
		feature = features(x, mz=mz),
		feature.groups,
		mz,
		plusminus,
		...)
	{
		if ( !missing(formula) && missing(feature) && missing(mz) )
			return(callNextMethod(as(x, "SparseImagingExperiment"), formula=formula, ...))
		if ( (!missing(feature) || !missing(mz)) && missing(feature.groups) ) {
			if ( missing(mz) )
				mz <- Cardinal::mz(x)[feature]
			if ( missing(plusminus) || all(plusminus == 0) ) {
				if ( is.null(featureNames(x)) ) {
					feature.groups <- .format.mz(mz(x)[feature])
				} else {
					feature.groups <- featureNames(x)[feature]
				}
			} else {
				feature.groups <- paste0(.format.mz(mz), " \u00b1 ", abs(plusminus))
				dmz <- abs(plusminus)
				feature.list <- lapply(seq_along(mz), function(i) {
					mzi <- mz[i]
					f <- which(mz(x) >= mzi - dmz & mz(x) <= mzi + dmz)
					if ( length(f) == 0L ) {
						mznew <- mz(x)[features(x, mz=mzi)]
						.warning("no features in range; re-centering m/z ", mzi, " to ", mznew)
						feature.groups[i] <- paste0(.format.mz(mznew), " \u00b1 ", dmz)
						f <- which(mz(x) > mznew - dmz & mz(x) < mznew + dmz)
					}
					f
				})
				feature.groups <- rep.int(feature.groups, lengths(feature.list))
				feature <- unlist(feature.list)
			}
		}
		if ( missing(feature.groups) ) {
			feature.groups <- NULL
		} else {
			feature.groups <- .try_eval(substitute(feature.groups),
				envir=as.env(featureData(x),
				enclos=environment(formula)))
		}
		callNextMethod(as(x, "SparseImagingExperiment"),
			formula=formula,
			feature=feature,
			feature.groups=feature.groups,
			...)
	})
