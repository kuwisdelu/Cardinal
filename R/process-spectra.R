
#### Spectral processing ####
## --------------------------

## Normalization

setMethod("normalize", "SpectralImagingData",
	function(object,
		method = c("tic", "rms", "reference"), ...)
{
	method <- match.arg(method)
	FUN <- switch(method,
		tic=.normalize_tic,
		rms=.normalize_rms,
		reference=.normalize_ref)
	addProcessing(object, FUN, label="intensity normalization", ...)
})

.normalize_tic <- function(x, t, ..., tic = length(x))
{
	auc <- sum(abs(x), na.rm=TRUE)
	if ( auc > 0 ) {
		y <- tic * x / auc
	} else {
		y <- rep.int(0, length(x))
	}
	replace(y, is.na(y), 0)
}

.normalize_rms <- function(x, t, ..., rms = 1)
{
	qm <- sqrt(mean(x^2, na.rm=TRUE))
	if ( qm > 0 ) {
		y <- rms * x / qm
	} else {
		y <- rep.int(0, length(x))
	}
	replace(y, is.na(y), 0)
}

.normalize_ref <- function(x, t, ..., i = 1L, scale = 1)
{
	if ( "feature" %in% ...names() ) {
		.Deprecated(old="feature", new="i")
		i <- list(...)$feature
	}
	ref <- x[i]
	if ( ref > 0 ) {
		y <- scale * x / ref
	} else {
		y <- rep.int(0, length(x))
	}
	replace(y, is.na(y), 0)
}


## Smoothing

setMethod("smooth", "SpectralImagingData",
	function(x,
		method = c("gaussian", "bilateral", "adaptive",
			"diff", "guide", "pag", "sgolay", "ma"), ...)
{
	method <- match.arg(method)
	FUN <- switch(method,
		gaussian=function(x, t, ...) matter::filt1_gauss(x, ...),
		bi=function(x, t, ...) matter::filt1_bi(x, ...),
		adaptive=function(x, t, ...) matter::filt1_adapt(x, ...),
		diff=function(x, t, ...) matter::filt1_diff(x, ...),
		guide=function(x, t, ...) matter::filt1_guide(x, ...),
		pag=function(x, t, ...) matter::filt1_pag(x, ...),
		sgolay=function(x, t, ...) matter::filt1_sg(x, ...),
		ma=function(x, t, ...) matter::filt1_ma(x, ...))
	addProcessing(x, FUN, label="smoothing", ...)
})


## Baseline reduction

setMethod("reduceBaseline", "SpectralImagingData",
	function(object, method = c("locmin", "hull", "snip", "median"), ...)
{
	method <- match.arg(method)
	FUN <- switch(method,
		locmin=function(x, t, ...) pmax(x - matter::estbase_loc(x, ...), 0),
		hull=function(x, t, ...) pmax(x - matter::estbase_hull(x, ...), 0),
		snip=function(x, t, ...) pmax(x - matter::estbase_snip(x, ...), 0),
		median=function(x, t, ...) pmax(x - matter::estbase_med(x, ...), 0))
	addProcessing(object, FUN, label="baseline reduction", ...)
})

