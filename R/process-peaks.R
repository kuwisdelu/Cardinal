
#### Peak processing ####
## ----------------------

## Peak picking

setMethod("peakPick", c("SpectralImagingData", "missing"),
	function(object,
		method = c("diff", "quantile", "filter", "cwt", "sd", "mad"),
		SNR = 2, type = c("height", "area"), ...)
{
	if ( hasMethod(centroided, class(object)) ) {
		if ( isTRUE(centroided(object)) ) {
			stop("object is already centroided")
		} else {
			centroided(object) <- TRUE
		}
	}
	method <- match.arg(method)
	type <- match.arg(type)
	if ( method == "cwt" ) {
		FUN <- .peakPick_cwt
	} else {
		FUN <- .peakPick
	}
	addProcessing(object, FUN, label=paste0(type, " peak picking"),
		method=method, SNR=SNR, type=type, ...)
})

.peakPick <- function(x, t, method, ..., SNR = 2, type = "height")
{
	peaks <- matter::findpeaks(x, noise=method, snr=SNR, relheight=0, ...)
	switch(type,
		height=cbind(t[peaks], x[peaks]),
		area=cbind(t[peaks], matter::peakareas(x, peaks)),
		stop("invalid peak type: ", sQuote(type)))
}

.peakPick_cwt <- function(x, t, ..., SNR = 2, type = "height")
{
	peaks <- matter::findpeaks_cwt(x, snr=SNR, ...)
	switch(type,
		height=cbind(t[peaks], x[peaks]),
		area=cbind(t[peaks], matter::peakareas(x, peaks)),
		stop("invalid peak type: ", sQuote(type)))
}

