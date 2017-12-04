require(testthat)
require(Cardinal)

context("ImagingExperiment class")

test_that("ImagingExperiment validity", {
	
	expect_error(new("ImagingExperiment"))

	setClass("XImagingExperiment", contains="ImagingExperiment")

	expect_true(validObject(new("XImagingExperiment")))

})

test_that("ImagingExperiment accessors", {

	setClass("XImagingExperiment", contains="ImagingExperiment")

	data <- matrix(1:9, nrow=3)
	t <- seq_len(9)
	a <- seq_len(9)

	idata <- ImageList(data)
	fdata <- DataFrame(t=t)
	pdata <- DataFrame(a=a)

	x <- new("XImagingExperiment",
		imageData=idata,
		featureData=fdata,
		elementMetadata=pdata)

	expect_true(validObject(x))

	expect_equal(imageData(x), idata)
	expect_equal(iData(x), idata[[1]])
	
	expect_equal(pixelData(x), pdata)
	expect_equal(pData(x), pdata)
	
	expect_equal(featureData(x), fdata)
	expect_equal(fData(x), fdata)

	expect_equal(dim(x), c(Features=nrow(fdata), Pixels=nrow(pdata)))

	expect_equal(x[["a"]], pdata[["a"]])

	expect_equal(x$a, pdata$a)

})
