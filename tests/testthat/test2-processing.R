require(testthat)
require(Cardinal)

context("new processing")

test_that("Cardinalv2 delayed processing", {

	options(Cardinal.progress=FALSE, Cardinal.verbose=FALSE)

	register(SerialParam())

	set.seed(1)
	data <- matrix(c(NA, NA, 1, 1, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA, 
		NA, NA, NA, NA, NA, 0, 1, 1, NA, NA, NA, NA, NA, 1, 0, 0, 1, 
		1, NA, NA, NA, NA, NA, 0, 1, 1, 1, 1, NA, NA, NA, NA, 0, 1, 1, 
		1, 1, 1, NA, NA, NA, NA, 1, 1, 1, 1, 1, 1, 1, NA, NA, NA, 1, 
		1, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA, NA, NA, NA), nrow=9, ncol=9)

	msset <- generateImage(data, range=c(1001, 5000), step=0.5, resolution=100, as="MSImageSet")

	msset <- as(msset, "MSImagingExperiment")

	tmp <- process(msset, function(s) log2(abs(s)))

	expect_true(validObject(tmp))

	tmp1 <- msset %>% process(abs, delay=TRUE)

	tmp2 <- tmp1 %>% process(log2, delay=TRUE)

	tmp3 <- process(tmp2)

	expect_true(validObject(tmp3))	

})
