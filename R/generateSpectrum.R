
#### Returns a simulated signal spectrum with peaks ####
## 'n' is the number of spectra
## 'peaks' is the number of peaks
## 'range' is the feature range
## 'centers' are the locations of the peaks
## 'intensities' are the intensities of the peaks
## 'step' is the step-size between measurements
## 'resolution' is the resolution of the instrument
## 'noise' is a scale-less coefficient of noise
## 'baseline' is a scale-less coefficient of baseline
## 'auc' is true if the intensities is measured as area under the curve
##-----------------------------------------------
generateSpectrum <- function(n, peaks = 100,
	range = c(1001, 20000),
	centers = seq(
		from = range[1] + diff(range) / (peaks + 1),
		to = range[2] - diff(range) / (peaks + 1),
		length.out = peaks),
	intensities = runif(peaks, min=0.1, max=1),
	step = diff(range)/1e3,
	resolution = 500,
	noise = 0.05,
	sd = 0.1,
	baseline = 2000,
	auc = TRUE)
{
	t <- seq(from=range[1], to=range[2], by=step)
	if ( missing(peaks) ) {
		if ( !missing(centers) ) {
			peaks <- length(centers)
		} else if ( !missing(intensities) ) {
			peaks <- length(intensities)
		}
	}
	if ( n > 1 ) {
		x <- sapply(rep(1, n), function(ns) {
			generateSpectrum(n=ns, peaks=peaks, range=range,
				centers=force(centers), intensities=force(intensities),
				step=step, resolution=resolution, noise=noise,
				sd=sd, baseline=baseline, auc=auc)$x
		} )
	} else {
		sigma <- (centers / resolution) / (2 * sqrt(2 * log(2)))
		intensities <- intensities + rnorm(peaks, 0, sd)
		x <- mapply(function(mus, ints, sigmas) {
			xs <- dnorm(t, mean=mus, sigmas)
			if ( !auc ) xs <- xs / max(xs, na.rm=TRUE)
			xs <- ints * xs
			xs
		}, centers, intensities, sigma)
		x <- length(t) * rowSums(x) / sum(x)
		x <- x + rnorm(length(t), mean=0, sd=noise * sd(x))
		x <- x - min(x)
		x <- length(t) * x / sum(x)
		baseline <- exp(-t/baseline)
		if ( max(baseline) > 0 ) baseline <- baseline / max(baseline)
		baseline <- (max(x) / 4) * baseline + rnorm(length(t), mean=0, sd=baseline)
		x <- x + baseline
	}
	list(x=x, t=t)
}
