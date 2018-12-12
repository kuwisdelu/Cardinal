
# Simulate a mass spectrum
simulateSpectrum <- function(n = 1L, peaks = 50L,
	mz = rlnorm(peaks, 7, 0.3), intensity = rlnorm(peaks, 1, 0.9),
	from = 0.9 * min(mz), to = 1.1 * max(mz), by = 400,
	sdpeaks = sdpeakmult * log1p(intensity), sdpeakmult = 0.2,
	sdnoise = 0.1, sdmz = 10, resolution = 1000, fmax = 0.5,
	baseline = 0, decay = 10, units=c("ppm", "mz"),
	representation = c("profile", "centroid"))
{
	if ( length(mz) != length(intensity) )
		stop("length of mz and peaks must match")
	units <- match.arg(units)
	representation <- match.arg(representation)
	m <- mz(from=from, to=to, by=by, units=units)
	if ( n > 1L ) {
		x <- replicate(n, simulateSpectrum(mz=mz, intensity=mz,
			from=from, to=to, by=by, sdpeaks=sdpeaks, sdpeakmult=sdpeakmult,
			sdnoise=sdnoise, resolution=resolution, fmax=fmax,
			baseline=baseline, decay=decay, units=units)$intensity)
	} else {
		dmz <- mz / resolution
		sdwidth <- qnorm(1 - fmax / 2) * dmz
		sdpeaks <- rep_len(sdpeaks, peaks)
		mzerr <- rnorm(1) * switch(units, ppm=1e-6 * mz * sdmz, mz=sdmz)
		mzobs <- mz + mzerr
		b <- baseline * exp(-(decay/max(m)) * (m - min(m)))
		x <- .simulateSpectrum(mzobs, intensity,
			peakwidth=sdwidth, sdpeaks=sdpeaks, sdnoise=sdnoise,
			mzrange=c(from, to), mzout=m)
		x <- pmax(b + x, 0)
		if ( representation == "centroid" ) {
			x <- approx(m, x, mz)$y
			m <- mz
		}
	}
	list(mz=m, intensity=x)
}

.simulateSpectrum <- function(mz, intensity,
	peakwidth, sdpeaks, sdnoise, mzrange, mzout)
{
	x <- numeric(length(mzout))
	for ( i in seq_along(mz) ) {
		if ( intensity[i] <= 0 || mz[i] < mzrange[1] || mz[i] > mzrange[2] )
			next
		xi <- dnorm(mzout, mean=mz[i], sd=peakwidth[i])
		intensityerr <- rlnorm(1, sdlog=sdpeaks[i])
		intensityerr <- intensityerr - exp(sdpeaks[i]^2 / 2)
		yi <- intensity[i] + intensityerr
		x <- x + yi * (xi / max(xi))
	}
	noise <- rlnorm(length(x), sdlog=sdnoise)
	noise <- noise - exp(sdnoise^2 / 2)
	x + noise
}
