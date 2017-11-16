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

	data <- matrix(1:4, nrow=2)
	t <- seq_len(10)
	coord <- expand.grid(x=1:3, y=1:3)
	status <- rbinom(nrow(coord), 1, 0.5)

	idata <- ImageList(data)
	fdata <- DataFrame(t=t)
	pdata <- PositionDataFrame(coord=coord, status=status)

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

	expect_equal(dims(x), dims(pdata))

	expect_equal(coord(x), coord(pdata))

	expect_equal(x[["status"]], pdata[["status"]])

	expect_equal(x$status, pdata$status)

})
