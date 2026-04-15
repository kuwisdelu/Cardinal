
## Mass recalibration

setMethod("recalibrate", "MSImagingArrays",
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
		addProcessing(object, .recalibrate_FUN(method),
			id="m/z calibration", ...,
			ref=ref, tol=tol, tol.ref=tol.ref)
	})

.recalibrate_FUN <- function(method)
{
	switch(method,
		locmax = function(x, ref, ...) {
			x$intensity <- pmax(0, matter::warp1_loc(x$intensity,
				tx=x$mz, ty=ref, n=length(x$mz), events="max", ...))
			x
		},
		dtw = function(x, ref, ...) {
			x$intensity <- pmax(0, matter::warp1_dtw(x$intensity,
				tx=x$mz, ty=ref, n=length(x$mz), ...))
			x
		},
		cow = function(x, ref, ...) {
			x$intensity <- pmax(0, matter::warp1_cow(x$intensity,
				tx=x$mz, ty=ref, n=length(x$mz), ...))
			x
		})
}
