
#### Peak picking ####
## --------------------

setMethod("peakPick", "MSImagingArrays",
	function(object, ref,
		method = c("diff", "sd", "mad", "quantile", "filter", "cwt"),
		SNR = 2, type = c("height", "area"),
		tolerance = NA, units = c("relative", "absolute"), ...)
	{
		method <- match.arg(method)
		type <- match.arg(type)
		if ( missing(ref) || is.null(ref) ) {
			if ( !is.na(tolerance) )
				.Warn("no 'ref' given so 'tolerance' will be ignored")
			if ( method == "cwt" ) {
				FUN <- .peakPickCWT
			} else {
				FUN <- .peakPick
			}
			addProcessing(object, FUN=FUN,
				id=paste0(type, " peak picking"), ...,
				method=method, SNR=SNR, type=type)
		} else {
			if ( is(ref, "MSImagingExperiment") || is(ref, "MassDataFrame") )
				ref <- mz(ref)
			if ( missing(units) && !missing(tolerance) )
				units <- get_units_from_names(tolerance, units)
			units <- match.arg(units)
			if ( is.unsorted(ref) )
				ref <- sort(ref)
			tol.ref <- switch(units, relative="x", absolute="abs")
			if ( is.na(tolerance) ) {
				tol <- 0.5 * estres(ref, ref=tol.ref)
			} else {
				tol <- tolerance
			}
			addProcessing(object, FUN=.peakBin,
				id=paste0(type, " peak picking"), ...,
				ref=ref, tol=tol, tol.ref=tol.ref, type=type)
		}
	})

.peakPick <- function(x, method, ..., SNR = 2, type = "height")
{
	peaks <- matter::findpeaks(x$intensity,
		noise=method, snr=SNR, relheight=NULL, ...)
	if ( type == "height" ) {
		values <- x$intensity[peaks]
	} else if ( type == "area" ) {
		values <- matter::peakareas(x$intensity, peaks, domain=x$mz)
	} else {
		.Error("invalid peak type: ", sQuote(type))
	}
	list(mz=x$mz[peaks], intensity=values)
}

.peakPickCWT <- function(x, method, ..., SNR = 2, type = "height")
{
	peaks <- matter::findpeaks_cwt(x$intensity, snr=SNR, ...)
	if ( type == "height" ) {
		values <- x$intensity[peaks]
	} else if ( type == "area" ) {
		values <- matter::peakareas(x$intensity, peaks, domain=x$mz)
	} else {
		.Error("invalid peak type: ", sQuote(type))
	}
	list(mz=x$mz[peaks], intensity=values)
}

.peakBin <- function(x, ref, tol, tol.ref,..., type = "height")
{
	peaks <- matter::findpeaks(x$intensity, relheight=NULL, bounds=FALSE, ...)
	hits <- bsearch(ref, x$mz[peaks], tol=tol, tol.ref=tol.ref)
	nz <- !is.na(hits)
	nzpeaks <- peaks[hits[nz]]
	values <- numeric(length(ref))
	if ( type == "height" ) {
		values[nz] <- matter::peakheights(x$intensity, nzpeaks)
	} else if ( type == "area" ) {
		values[nz] <- matter::peakareas(x$intensity, nzpeaks, domain=x$mz)
	} else {
		.Error("invalid peak type: ", sQuote(type))
	}
	list(mz=ref, intensity=values)
}
