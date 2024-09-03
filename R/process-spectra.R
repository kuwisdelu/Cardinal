
#### Spectral processing ####
## --------------------------

## Normalization

setMethod("normalize", "MSImagingExperiment_OR_Arrays",
	function(object,
		method = c("tic", "rms", "reference"),
		scale = NA, ref = NULL, ...)
{
	method <- match.arg(method)
	if ( is.na(scale) ) {
		if ( method == "reference" ) {
			if ( is.null(ref) )
				.Error("must provide 'ref' for method='reference'")
			scale <- 1
		} else {
			if ( is.null(dim(object)) ) {
				scale <- max(lengths(mz(object)))
			} else {
				scale <- length(mz(object))
			}
		}
	}
	FUN <- .normalize_fun[[method, exact=FALSE]]
	if ( method == "reference" ) {
		addProcessing(object, FUN,
			label="intensity normalization",
			metadata=list(method=method),
			scale=scale, ref=ref, ...)
	} else {
		addProcessing(object, FUN,
			label="intensity normalization",
			metadata=list(method=method),
			scale=scale, ...)
	}
})

setMethod("normalize", "SpectralImagingData",
	function(object,
		method = c("tic", "rms", "reference"), ...)
{
	method <- match.arg(method)
	FUN <- .normalize_fun[[method, exact=FALSE]]
	addProcessing(object, FUN,
		label="intensity normalization",
		metadata=list(method=method), ...)
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
	method <- match.arg(method)
	FUN <- .smooth_fun[[method, exact=FALSE]]
	addProcessing(x, FUN,
		label="smoothing",
		metadata=list(method=method), ...)
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
	method <- match.arg(method)
	FUN <- .reduceBaseline_fun[[method, exact=FALSE]]
	addProcessing(object, FUN,
		label="baseline reduction",
		metadata=list(method=method), ...)
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
	method <- match.arg(method)
	if ( !missing(ref) ) {
		if ( is(ref, "MSImagingExperiment") || is(ref, "MassDataFrame") )
			ref <- mz(ref)
	}
	if ( missing(units) && !missing(tolerance) )
		units <- get_units_from_names(tolerance, units)
	units <- match.arg(units)
	if ( is.na(tolerance) ) {
		tol <- estres(ref, ref=switch(units, ppm="x", mz="abs"))
	} else {
		tol <- switch(units, ppm=1e-6 * tolerance, mz=tolerance)
	}
	tol.ref <- switch(units, ppm="x", mz="abs")
	FUN <- .recalibrate_fun[[method, exact=FALSE]]
	addProcessing(object, FUN,
		label="m/z calibration",
		metadata=list(method=method),
		ref=ref, tol=tol, tol.ref=tol.ref, ...)
})

setMethod("recalibrate", "SpectralImagingData",
	function(object, ref,
		method = c("locmax", "dtw", "cow"),
		tolerance = NA, units = c("relative", "absolute"), ...)
{
	method <- match.arg(method)
	if ( !missing(ref) ) {
		if ( is(ref, "MSImagingExperiment") || is(ref, "MassDataFrame") )
			ref <- mz(ref)
	}
	if ( missing(units) && !missing(tolerance) )
		units <- get_units_from_names(tolerance, units)
	units <- match.arg(units)
	if ( is.na(tolerance) ) {
		tol <- estres(ref, ref=switch(units, relative="x", absolute="abs"))
	} else {
		tol <- tolerance
	}
	tol.ref <- switch(units, relative="x", absolute="abs")
	FUN <- .recalibrate_fun[[method, exact=FALSE]]
	addProcessing(object, FUN,
		label="recalibration",
		metadata=list(method=method),
		ref=ref, tol=tol, tol.ref=tol.ref, ...)
})

.recalibrate_fun <- list(
	locmax = function(x, t, ref, ...) 
		pmax(0, matter::warp1_loc(x, tx=t, ty=ref, n=length(x), events="max", ...)),
	dtw = function(x, t, ref, ...) 
		pmax(0, matter::warp1_dtw(x, tx=t, ty=ref, n=length(x), ...)),
	cow = function(x, t, ref, ...) 
		pmax(0, matter::warp1_cow(x, tx=t, ty=ref, n=length(x), ...)))

