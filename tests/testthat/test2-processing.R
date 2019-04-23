require(testthat)
require(Cardinal)

context("processing 2")

options(Cardinal.progress=interactive(), Cardinal.verbose=interactive())

register(SerialParam())

set.seed(1)
data <- simulateImage(preset=1, dim=c(10,10), baseline=1)
data_c <- data[,pData(data)$circle]

test_that("Cardinal >=2 delayed processing", {

	tmp <- process(data, function(s) log2(abs(s)))

	expect_true(validObject(tmp))

	tmp1 <- data %>% process(abs, delay=TRUE)

	tmp2 <- tmp1 %>% process(log2, delay=TRUE)

	tmp3 <- process(tmp2)

	expect_true(validObject(tmp3))

})

test_that("pre-processing 2", {

	tmp <- normalize(data_c, method="rms") %>% process()

	expect_true(validObject(tmp))

	tmp2 <- smoothSignal(tmp, method="gaussian") %>% process()

	expect_true(validObject(tmp2))

	tmp3 <- reduceBaseline(tmp2, method="locmin") %>% process()

	expect_true(validObject(tmp3))

	tmp4 <- peakPick(tmp3, method="simple") %>% process()

	expect_true(validObject(tmp4))

	tmp5 <- peakAlign(tmp4, tolerance=400, units="ppm") %>% process()

	expect_true(validObject(tmp5))

	tmp6 <- peakFilter(tmp5, freq.min=0.1) %>% process()

	expect_true(validObject(tmp6))

	data_p <- peakBin(tmp3, ref=mz(tmp6), tolerance=1, units="mz") %>% process()

	data_b <- mzBin(tmp3, to=800, resolution=400, units="ppm") %>% process()

})

