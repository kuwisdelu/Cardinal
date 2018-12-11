
# Generate a sequence of m/z values

setMethod("mz", "missing",
	function(from, to, by = 400, units = c("ppm", "mz"), ...) {
		units <- match.arg(units)
		halfwidth <- by / 2
		mz <- switch(units,
			ppm = seq.ppm(from=from, to=to, ppm=halfwidth),
			mz = seq(from=from, to=to, by=halfwidth))
		tol <- switch(units,
			ppm = c(relative = halfwidth * 1e-6),
			mz = c(absolute = halfwidth))
		attr(mz, "tolerance") <- tol
		mz
	})
