require(testthat)
require(Cardinal)

context("ImageList class")

test_that("ImageList validity", {
	
	expect_true(validObject(new("SimpleImageList")))

	data0 <- matrix(1:4, nrow=2)
	idata <- ImageList(data0)
	expect_true(validObject(idata))

})

test_that("ImageList accessors", {

	data0 <- matrix(1:4, nrow=2)
	idata <- ImageList(data0)
	expect_equal(idata[[1]], data0)

	data1 <- matrix(5:8, nrow=2)
	idata[["data1"]] <- data1
	expect_equal(idata[["data1"]], data1)
	
	data2 <- matrix(9:12, nrow=2)
	idata2 <- ImageList(list(d0=data0, d1=data1, d2=data2))
	expect_equal(idata2[["d1"]], data1)
	
	expect_equal(names(idata2), c("d0", "d1", "d2"))
	expect_equal(length(idata2), 3)
	expect_equal(dim(idata2), dim(data0))

})
