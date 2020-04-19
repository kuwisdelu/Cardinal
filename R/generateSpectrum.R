
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
	.Defunct("simulateSpectrum")
}
