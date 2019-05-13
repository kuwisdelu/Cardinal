require(testthat)
require(Cardinal)
require(EBImage)

context("AnnotatedImagingExperiment class")

test_that("AnnotatedImagingExperiment validity", {
	
	expect_true(validObject(new("AnnotatedImagingExperiment")))

	x <- Image(rnorm(30*30*3), dim=c(30,30,3), colormode='Color')

	fdata <- DataFrame(col=c("r", "g", "b"))
	pdata <- DataFrame(f="1.jpg")

	y <- AnnotatedImagingExperiment(x, fdata, pdata)

	expect_true(validObject(y))

})

test_that("AnnotatedImagingExperiment accessors", {

	x <- Image(rnorm(30*30*3), dim=c(30,30,3), colormode='Color')
	x <- as(x, "AnnotatedImage")

	idata <- AnnotatedImageList(A=x, B=x, C=x)
	fdata <- DataFrame(col=c("r", "g", "b"))
	pdata <- DataFrame(f=c("1.jpg", "2.jpg", "3.jpg"))

	y <- AnnotatedImagingExperiment(idata, fdata, pdata)

	expect_equal(iData(y), x)
	expect_equal(pData(y), pdata)
	expect_equal(fData(y), fdata)

	expect_equivalent(coord(y), matrix(0, nrow=2, ncol=3))
	expect_equivalent(resolution(y), c(1, 1, 1))
	expect_equivalent(height(y), c(30, 30, 30))
	expect_equivalent(width(y), c(30, 30, 30))

})

