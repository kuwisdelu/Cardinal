
# Generate a sequence of m/z values

setMethod("mz", "missing",
	function(from, to, by, resolution = 200, units = c("ppm", "mz"), ...)
	{
		units <- match.arg(units)
		if ( missing(by) )
			by <- switch(units, ppm=resolution * 2, mz=resolution)
		halfwidth <- by / 2
		mz <- switch(units,
			ppm = seq.ppm(from=from, to=to, ppm=halfwidth),
			mz = seq(from=from, to=to, by=2 * halfwidth))
		tol <- switch(units,
			ppm = c(relative = halfwidth * 1e-6),
			mz = c(absolute = halfwidth))
		attr(mz, "tolerance") <- tol
		mz
	})
