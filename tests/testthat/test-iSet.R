require(testthat)
require(Cardinal)

context("iSet class")

test_that("iSet validity", {
	
	expect_error(new("iSet"))

	setClass("TestSet", contains="iSet")

	expect_true(validObject(new("TestSet")))

	test <- new("TestSet",
		pixelData=IAnnotatedDataFrame(
			data.frame(pixels=1:100)),
		featureData=Biobase::AnnotatedDataFrame(
			data.frame(features=1:20)))

	expect_true(validObject(test))

})
