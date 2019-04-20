require(testthat)
require(Cardinal)

context("processing 2")

options(Cardinal.progress=interactive(), Cardinal.verbose=interactive())

register(SerialParam())

test_that("Cardinal >=2 delayed processing", {

	data <- simulateImage(preset=1, dim=c(10,10))

	tmp <- process(data, function(s) log2(abs(s)))

	expect_true(validObject(tmp))

	tmp1 <- data %>% process(abs, delay=TRUE)

	tmp2 <- tmp1 %>% process(log2, delay=TRUE)

	tmp3 <- process(tmp2)

	expect_true(validObject(tmp3))

})
