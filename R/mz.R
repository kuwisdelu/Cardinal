
# Generate a sequence of m/z values

setMethod("mz", "missing",
	function(from, to, by = resolution, resolution = 200, units = c("ppm", "mz"), ...)
	{
		units <- match.arg(units)
		halfwidth <- unname(by / 2)
		mz <- switch(units,
			ppm = seq.ppm(from=from, to=to, ppm=halfwidth),
			mz = seq(from=from, to=to, by=2 * halfwidth))
		tol <- switch(units,
			ppm = c(relative = 2 * halfwidth * 1e-6),
			mz = c(absolute = 2 * halfwidth))
		res <- switch(units,
			ppm = c(ppm = 2 * halfwidth),
			mz = c(mz = 2 * halfwidth))
		attr(mz, "tolerance") <- tol
		attr(mz, "resolution") <- res
		mz
	})
