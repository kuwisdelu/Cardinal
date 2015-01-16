require(testthat)

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

test_that("ImageData combine", {

	data1 <- matrix(1:4, nrow=2, ncol=2, dimnames=list(1:2, 1:2))
	idata1 <- ImageData(data0=data1, storageMode="immutableEnvironment")

	data2 <- matrix(4:7, nrow=2, ncol=2, dimnames=list(2:3, 2:3))
	idata2 <- ImageData(data0=data2, storageMode="immutableEnvironment")

	combdata <- combine(idata1, idata2)
	expect_equivalent(combdata[["data0"]], combine(data1, data2))

	data3 <- matrix(4:9, nrow=2, ncol=3, dimnames=list(2:3, 2:4))
	idata3 <- ImageData(data0=data3, storageMode="immutableEnvironment")
	multicombdata <- combine(idata1, idata2, idata3)
	expect_equivalent(multicombdata[["data0"]], combine(data1, data2, data3))
	
	data4 <- array(1:27, dim=rep(3,3), dimnames=rep(list(1:3), 3))
	idata4 <- ImageData(data0=data4)
	data5 <- array(1:27, dim=rep(3,3), dimnames=rep(list(c(1,2,4)), 3))
	idata5 <- ImageData(data0=data5)
	arrcombdata <- combine(idata4, idata5)
	expect_true(validObject(arrcombdata))
	expect_equivalent(arrcombdata[["data0"]], combine(data4, data5))

})
