require(testthat)
require(Cardinal)
require(EBImage)

context("AnnotatedImagingExperiment class")

test_that("AnnotatedImagingExperiment validity", {
	
	expect_true(validObject(new("AnnotatedImageList")))

	x <- Image(rnorm(30*30*3), dim=c(30,30,3), colormode='Color')

})

test_that("AnnotatedImagingExperiment accessors", {

	x <- Image(rnorm(30*30*3), dim=c(30,30,3), colormode='Color')
	x <- as(x, "AnnotatedImage")

})

