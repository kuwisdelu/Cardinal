
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
		sd <- qnorm(1 - fmax / 2) * dmz
		x <- numeric(length(m))
		x <- x + baseline * exp(-(decay/max(m)) * (m - min(m)))
		for ( i in seq_along(intensity) ) {
			if ( intensity[i] <= 0 )
				next
			err <- switch(units, ppm=1e-6 * sdmz * mz[i], mz=sdmz)
			mzi <- mz[i] + rnorm(1, sd=err)
			xi <- dnorm(m, mzi, sd[i])
			peakerr <- rlnorm(1, sdlog=sdpeaks)
			yi <- intensity[i] + (peakerr - min(peakerr))
			x <- x + yi * (xi / max(xi))
		}
		spectrumerr <- rlnorm(length(x), sdlog=sdnoise)
		x <- x + (spectrumerr - min(spectrumerr))
		if ( representation == "centroid" ) {
			x <- approx(m, x, mz)$y
			m <- mz
		}
	}
	list(mz=m, intensity=x)
}
