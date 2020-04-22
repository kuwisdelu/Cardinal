require(testthat)
require(Cardinal)

context("processing-2")

set.seed(2)
data <- simulateImage(preset=1, dim=c(10,10), baseline=1)
data_c <- data[,pData(data)$circle]

set.seed(2)
data_r <- simulateImage(preset=7, peakheight=5, dim=c(10,10), representation="centroid")

test_that("Cardinal >=2 delayed processing", {

	tmp <- process(data, function(s) log2(s + 1))

	expect_true(validObject(tmp))

	plus1 <- function(x) x + 1

	tmp1 <- data %>% process(plus1, delay=TRUE)

	tmp2 <- tmp1 %>% process(log2, delay=TRUE)

	tmp3 <- process(tmp2)

	expect_true(validObject(tmp3))

	expect_equal(iData(tmp), iData(tmp3))

})

test_that("pre-processing 2 continuous", {

	tmp <- normalize(data_c, method="rms") %>% process()

	expect_true(validObject(tmp))

	tmp2 <- smoothSignal(tmp, method="gaussian") %>% process()

	expect_true(validObject(tmp2))

	tmp3 <- reduceBaseline(tmp2, method="locmin") %>% process()

	expect_true(validObject(tmp3))

	tmp4 <- peakPick(tmp3, method="mad") %>% process()

	expect_true(validObject(tmp4))

	tmp5 <- peakAlign(tmp4, tolerance=400, units="ppm") %>% process()

	expect_true(validObject(tmp5))

	tmp6 <- peakFilter(tmp5, freq.min=0.1) %>% process()

	expect_true(validObject(tmp6))

	data_p <- peakBin(tmp3, ref=mz(tmp6), tolerance=1, units="mz") %>% process()

	expect_true(validObject(data_p))

	data_b <- mzBin(tmp3, from=500, to=800, resolution=1000, units="ppm") %>% process()

	expect_true(validObject(data_b))

	data_f <- mzFilter(tmp3, rm.zero=TRUE) %>% process()

	expect_true(validObject(data_f))

	data_n <- normalize(data_r, method="reference", feature=nrow(data_r)) %>% process()

	expect_true(validObject(data_n))

})

test_that("pre-processing 2 processed", {

	data_c <- as(data_c, "MSProcessedImagingExperiment")

	tmp <- normalize(data_c, method="rms") %>% process()

	expect_true(validObject(tmp))

	tmp2 <- smoothSignal(tmp, method="gaussian") %>% process()

	expect_true(validObject(tmp2))

	tmp3 <- reduceBaseline(tmp2, method="locmin") %>% process()

	expect_true(validObject(tmp3))

	tmp4 <- peakPick(tmp3, method="mad") %>% process()

	expect_true(validObject(tmp4))

	tmp5 <- peakAlign(tmp4, tolerance=400, units="ppm") %>% process()

	expect_true(validObject(tmp5))

	tmp6 <- peakFilter(tmp5, freq.min=0.1) %>% process()

	expect_true(validObject(tmp6))

	data_p <- peakBin(tmp3, ref=mz(tmp6), tolerance=1, units="mz") %>% process()

	expect_true(validObject(data_p))

	data_b <- mzBin(tmp3, from=500, to=800, resolution=1000, units="ppm") %>% process()

	expect_true(validObject(data_b))

	data_f <- mzFilter(tmp3, rm.zero=TRUE) %>% process()

	expect_true(validObject(data_f))

})

