
#### Spectral processing ####
## --------------------------

## Normalization

setMethod("normalize", "SpectralImagingData",
	function(object,
		method = c("tic", "rms", "reference"), ...)
{
	FUN <- .normalize_fun[[match.arg(method), exact=FALSE]]
	addProcessing(object, FUN, label="intensity normalization", ...)
})

.normalize_fun <- list(
	tic = function(x, t, ...)
		matter::rescale_sum(x, ...),
	rms = function(x, t, ...)
		matter::rescale_rms(x, ...),
	reference = function(x, t, ...)
		matter::rescale_ref(x, ..., domain=t))


## Smoothing

setMethod("smooth", "SpectralImagingData",
	function(x,
		method = c("gaussian", "bilateral", "adaptive",
			"diff", "guide", "pag", "sgolay", "ma"), ...)
{
	FUN <- .smooth_fun[[match.arg(method), exact=FALSE]]
	addProcessing(x, FUN, label="smoothing", ...)
})

.smooth_fun <- list(
	gaussian = function(x, t, ...) 
		matter::filt1_gauss(x, ...),
	bi = function(x, t, ...) 
		matter::filt1_bi(x, ...),
	adaptive = function(x, t, ...) 
		matter::filt1_adapt(x, ...),
	diff = function(x, t, ...) 
		matter::filt1_diff(x, ...),
	guide = function(x, t, ...) 
		matter::filt1_guide(x, ...),
	pag = function(x, t, ...) 
		matter::filt1_pag(x, ...),
	sgolay = function(x, t, ...) 
		matter::filt1_sg(x, ...),
	ma = function(x, t, ...) 
		matter::filt1_ma(x, ...))

## Baseline reduction

setMethod("reduceBaseline", "SpectralImagingData",
	function(object,
		method = c("locmin", "hull", "snip", "median"), ...)
{
	FUN <- .reduceBaseline_fun[[match.arg(method), exact=FALSE]]
	addProcessing(object, FUN, label="baseline reduction", ...)
})

.reduceBaseline_fun <- list(
	locmin = function(x, t, ...) 
		pmax(0, x - matter::estbase_loc(x, ...)),
	hull = function(x, t, ...) 
		pmax(0, x - matter::estbase_hull(x, ...)),
	snip = function(x, t, ...) 
		pmax(0, x - matter::estbase_snip(x, ...)),
	median = function(x, t, ...) 
		pmax(0, x - matter::estbase_med(x, ...)))


## Recalibration

setMethod("recalibrate", "MSImagingExperiment_OR_Arrays",
	function(object, ref,
		method = c("locmax", "dtw", "cow"),
		tolerance = NA, units = c("ppm", "mz"), ...)
{
	units <- match.arg(units)
	if ( is.na(tolerance) ) {
		tol <- estres(ref, tol.ref=switch(units, ppm="x", mz="abs"))
	} else {
		tol <- switch(units, ppm=1e-6 * tolerance, mz=tolerance)
	}
	tol.ref <- switch(units, ppm="x", mz="abs")
	FUN <- .recalibrate_fun[[match.arg(method), exact=FALSE]]
	addProcessing(object, FUN, label="m/z calibration",
		ref=ref, tol=tol, tol.ref=tol.ref, ...)
})

setMethod("recalibrate", "SpectralImagingData",
	function(object, ref,
		method = c("locmax", "dtw", "cow"),
		tolerance = NA, units = c("relative", "absolute"), ...)
{
	units <- match.arg(units)
	if ( is.na(tolerance) ) {
		tol <- estres(ref, tol.ref=switch(units, relative="x", absolute="abs"))
	} else {
		tol <- tolerance
	}
	tol.ref <- switch(units, relative="x", absolute="abs")
	FUN <- .recalibrate_fun[[match.arg(method), exact=FALSE]]
	addProcessing(object, FUN, label="recalibration",
		ref=ref, tol=tol, tol.ref=tol.ref, ...)
})

.recalibrate_fun <- list(
	locmax = function(x, t, ref, ...) 
		matter::warp1_loc(x, tx=t, ty=ref, n=length(x), events="max", ...),
	dtw = function(x, t, ref, ...) 
		matter::warp1_dtw(x, tx=t, ty=ref, n=length(x), ...),
	cow = function(x, t, ref, ...) 
		matter::warp1_cow(x, tx=t, ty=ref, n=length(x), ...))

