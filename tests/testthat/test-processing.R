require(testthat)
require(Cardinal)

context("processing")

# options(Cardinal.progress=FALSE, Cardinal.verbose=FALSE)

# set.seed(1)
# data <- matrix(c(NA, NA, 1, 1, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA, 
# 	NA, NA, NA, NA, NA, 0, 1, 1, NA, NA, NA, NA, NA, 1, 0, 0, 1, 
# 	1, NA, NA, NA, NA, NA, 0, 1, 1, 1, 1, NA, NA, NA, NA, 0, 1, 1, 
# 	1, 1, 1, NA, NA, NA, NA, 1, 1, 1, 1, 1, 1, 1, NA, NA, NA, 1, 
# 	1, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA, NA, NA, NA), nrow=9, ncol=9)

# msset <- generateImage(data, range=c(2000, 3000), step=0.5, resolution=100, as="MSImageSet")

# plot <- FALSE

test_that("Cardinal <= 1 pre-processing", {

	# normalization

	# msset2 <- normalize(msset, method="tic", plot=plot)
	
	# expect_true(validObject(msset2))

	# # smoothing

	# msset3 <- smoothSignal(msset2, method="gaussian", window=20, plot=plot)

	# expect_true(validObject(msset3))

	# # baseline reduction

	# msset4 <- reduceBaseline(msset3, method="median", plot=plot)

	# expect_true(validObject(msset4))

	# # peak picking

	# msset5 <- peakPick(msset4, method="simple", SNR=6, plot=plot)

	# expect_true(validObject(msset5))

	# # peak alignment

	# msset6 <- peakAlign(msset5, method="diff", diff.max=500, plot=plot)

	# PeakFreq <- apply(spectra(msset6), 1, function(s) sum(s!=0))

	# expect_equal(sum(PeakFreq != 0), 2)

	# msset7 <- msset6[PeakFreq != 0,]

	# expect_true(validObject(msset7))

	# # reduce dimension

	# msset8 <- reduceDimension(msset2, msset7, method="peaks", plot=plot)
	
	# expect_true(validObject(msset8))	

})

test_that("Cardinal 1.x batch pre-processing", {
	
	# tmp <- batchProcess(msset,
	# 	normalize=TRUE,
	# 	smoothSignal=TRUE,
	# 	reduceBaseline=TRUE,
	# 	peakPick=TRUE,
	# 	layout=c(2,2), plot=plot)

	# expect_true(validObject(tmp))

	# tmp <- batchProcess(msset,
	# 	normalize=TRUE,
	# 	reduceBaseline=list(blocks=200),
	# 	reduceDimension=list(method="bin", width=10, units="mz"),
	# 	layout=c(1,3), plot=plot)

	# expect_true(validObject(tmp))

	# tmp <- batchProcess(msset,
	# 	normalize=TRUE,
	# 	reduceBaseline=list(blocks=200),
	# 	peakPick=list(SNR=12),
	# 	peakAlign=TRUE,
	# 	layout=c(1,3), plot=plot)

	# expect_true(validObject(tmp))

})
