require(testthat)

context("processing")

test_that("Cardinal pre-processing", {

	options(Cardinal.progress=FALSE, Cardinal.verbose=FALSE)

	set.seed(1)
	data <- matrix(c(NA, NA, 1, 1, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA, 
		NA, NA, NA, NA, NA, 0, 1, 1, NA, NA, NA, NA, NA, 1, 0, 0, 1, 
		1, NA, NA, NA, NA, NA, 0, 1, 1, 1, 1, NA, NA, NA, NA, 0, 1, 1, 
		1, 1, 1, NA, NA, NA, NA, 1, 1, 1, 1, 1, 1, 1, NA, NA, NA, 1, 
		1, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA, NA, NA, NA), nrow=9, ncol=9)

	msset <- generateImage(data, range=c(1001, 5000), step=0.5, resolution=100, as="MSImageSet")

	plot <- FALSE

	# normalization

	tmp <- normalize(msset, method="tic", pixel=c(1,10), plot=plot)
	expect_true(validObject(tmp))

	msset2 <- normalize(msset, method="tic", plot=plot)
	
	expect_true(validObject(msset2))

	# smoothing

	tmp <- smoothSignal(msset2, method="gaussian", pixel=c(1,10), window=25, plot=plot)
	expect_true(validObject(tmp))

	tmp <- smoothSignal(msset2, method="sgolay", pixel=c(1,10), window=30, plot=plot)
	expect_true(validObject(tmp))

	tmp <- smoothSignal(msset2, method="ma", pixel=c(1,10), window=10, plot=plot)
	expect_true(validObject(tmp))

	msset3 <- smoothSignal(msset2, method="gaussian", window=20, plot=plot)

	expect_true(validObject(msset3))

	# baseline reduction

	tmp <- reduceBaseline(msset3, method="median", pixel=c(1,10), plot=plot)
	expect_true(validObject(tmp))

	msset4 <- reduceBaseline(msset3, method="median", plot=plot)

	expect_true(validObject(msset4))

	# peak picking

	tmp <- peakPick(msset4, method="simple", pixel=c(1,10), plot=plot)
	expect_true(validObject(tmp))

	tmp <- peakPick(msset4, method="adaptive", pixel=c(1,10), plot=plot)
	expect_true(validObject(tmp))

	tmp <- peakPick(msset4, method="limpic", pixel=c(1,10), plot=plot)
	expect_true(validObject(tmp))

	msset4.5 <- peakPick(msset4, method="simple", pixel=c(1,10), SNR=6, plot=plot)

	expect_true(validObject(msset4.5))

	msset5 <- peakPick(msset4, method="simple", SNR=6, plot=plot)

	expect_true(validObject(msset5))

	# peak alignment

	tmp <- peakAlign(msset4.5, method="diff", pixel=1, diff.max=1000, plot=plot)
	expect_true(validObject(tmp))

	tmp <- peakAlign(msset4.5, method="diff", diff.max=1, units="mz", plot=plot)
	expect_true(validObject(tmp))

	tmp <- peakAlign(msset4.5, method="DP", gap=0.1, plot=plot)
	expect_true(validObject(tmp))

	tmp <- peakAlign(msset5, method="diff", pixel=c(1,10), diff.max=1000, plot=plot)
	expect_true(validObject(tmp))

	tmp <- peakAlign(msset5, method="DP", pixel=c(1,10), plot=plot)
	expect_true(validObject(tmp))

	msset6 <- peakAlign(msset5, method="diff", diff.max=500, plot=plot)

	PeakFreq <- apply(spectra(msset6), 1, function(s) sum(s!=0))

	expect_equal(sum(PeakFreq != 0), 2)

	msset7 <- msset6[PeakFreq != 0,]

	expect_true(validObject(msset7))

	# reduce dimension

	tmp <- reduceDimension(msset2, method="bin", pixel=c(1,10), width=10, units="mz", plot=plot)
	expect_true(validObject(tmp))

	tmp <- reduceDimension(msset2, method="bin", pixel=c(1,10), fun=mean, width=10, units="mz", plot=plot)
	expect_true(validObject(tmp))

	tmp <- reduceDimension(msset2, method="bin", pixel=c(1,10), width=1000, plot=plot)
	expect_true(validObject(tmp))

	tmp <- reduceDimension(msset2, method="resample", pixel=c(1,10), step=10, plot=plot)
	expect_true(validObject(tmp))

	tmp <- reduceDimension(msset2, msset7, method="peaks", pixel=c(1,10), plot=plot)
	expect_true(validObject(tmp))

	tmp <- reduceDimension(msset2, msset7, method="peaks", type="area", pixel=c(1,10), plot=plot)
	expect_true(validObject(tmp))

	msset8 <- reduceDimension(msset2, msset7, method="peaks", plot=plot)
	
	expect_true(validObject(msset8))	

})

test_that("Cardinal batch pre-processing", {

	options(Cardinal.progress=FALSE, Cardinal.verbose=FALSE)

	set.seed(1)
	data <- matrix(c(NA, NA, 1, 1, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA, 
		NA, NA, NA, NA, NA, 0, 1, 1, NA, NA, NA, NA, NA, 1, 0, 0, 1, 
		1, NA, NA, NA, NA, NA, 0, 1, 1, 1, 1, NA, NA, NA, NA, 0, 1, 1, 
		1, 1, 1, NA, NA, NA, NA, 1, 1, 1, 1, 1, 1, 1, NA, NA, NA, 1, 
		1, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA, NA, NA, NA), nrow=9, ncol=9)

	msset <- generateImage(data, range=c(1001, 5000), step=0.5, resolution=100, as="MSImageSet")

	plot <- FALSE
	
	tmp <- batchProcess(msset,
		normalize=TRUE,
		smoothSignal=TRUE,
		reduceBaseline=TRUE,
		peakPick=TRUE,
		layout=c(2,2), plot=plot)

	expect_true(validObject(tmp))

	tmp <- batchProcess(msset,
		normalize=TRUE,
		reduceBaseline=list(blocks=200),
		reduceDimension=list(method="bin", width=10, units="mz"),
		layout=c(1,3), plot=plot)

	expect_true(validObject(tmp))

	tmp <- batchProcess(msset,
		normalize=TRUE,
		reduceBaseline=list(blocks=200),
		peakPick=list(SNR=12),
		layout=c(1,3), plot=plot)

	expect_true(validObject(tmp))

	expect_error(tmp <- batchProcess(msset,
		normalize=TRUE,
		reduceBaseline=list(blocks=200),
		reduceDimension=list(method="bin", width=10, units="mz"),
		peakPick=list(SNR=12),
		layout=c(2,2), plot=plot))

	tmp <- batchProcess(msset,
		normalize=TRUE,
		reduceBaseline=list(blocks=200),
		peakPick=list(SNR=12),
		peakAlign=TRUE,
		layout=c(1,3), plot=plot)

	expect_true(validObject(tmp))

})
