require(testthat)
require(Cardinal)

context("XDataFrame class")

test_that("XDataFrame basics", {
	
	expect_true(validObject(new("XDataFrame")))

	xdf <- XDataFrame(x=1:10, y=11:20)

	xdf$z <- 21:30

	expect_true(validObject(xdf))

	xdf$z <- NULL

	expect_true(validObject(xdf))

})
