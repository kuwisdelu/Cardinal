require(testthat)
require(Cardinal)

context("ImageData class")

test_that("ImageData validity", {
	
	expect_true(validObject(new("ImageData")))
	expect_true(validObject(ImageData()))

	data0 <- matrix(1:4, nrow=2)
	idata <- ImageData(data0=data0)
	expect_true(validObject(idata))

	expect_error(ImageData(data0), "all elements must be named")

})

test_that("ImageData accessors", {

	data0 <- matrix(1:4, nrow=2)
	idata <- ImageData(data0=data0, storageMode="immutableEnvironment")
	expect_equal(idata[["data0"]], data0)

	data1 <- matrix(5:8, nrow=2)
	idata[["data1"]] <- data1
	expect_equal(idata[["data1"]], data1)
	
	data2 <- matrix(9:12, nrow=2)
	storageMode(idata) <- "lockedEnvironment"
	expect_error(idata[["data2"]] <- data2)

})

test_that("ImageData names", {

	data0 <- matrix(1:4, nrow=2)
	idata <- ImageData(data0=data0, storageMode="immutableEnvironment")

	data1 <- matrix(5:8, nrow=2)
	idata[["data1"]] <- data1

	expect_equivalent(names(idata), c("data0", "data1"))

	names(idata) <- c("a", "b")

	expect_equivalent(names(idata), c("a", "b"))	

})
