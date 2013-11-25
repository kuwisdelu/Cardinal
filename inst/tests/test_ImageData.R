context("ImageData class")

test_that("ImageData validity", {
	
	expect_true(validObject(new("ImageData")))

	data0 <- matrix(1:4, nrow=2)
	idata <- ImageData(data0=data0)
	expect_that(validObject(idata), is_true())

	expect_that(ImageData(data0), throws_error("all elements must be named"))

})

test_that("ImageData manipulation", {

	data0 <- matrix(1:4, nrow=2)
	idata <- ImageData(data0=data0, storageMode="immutableEnvironment")
	expect_that(idata[["data0"]], equals(data0))

	data1 <- matrix(5:8, nrow=2)
	idata[["data1"]] <- data1
	expect_that(idata[["data1"]], equals(data1))
	
	data2 <- matrix(9:12, nrow=2)
	storageMode(idata) <- "lockedEnvironment"
	expect_that(idata[["data2"]] <- data2, throws_error())

})
