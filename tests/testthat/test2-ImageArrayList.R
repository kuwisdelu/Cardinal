require(testthat)
require(Cardinal)

context("ImageArrayList class")

test_that("ImageArrayList validity", {
	
	expect_true(validObject(new("SimpleImageArrayList")))

	data0 <- matrix(1:4, nrow=2)
	idata <- ImageArrayList(data0)
	expect_true(validObject(idata))

	data1 <- matrix(1:6, nrow=2)
	expect_error(ImageArrayList(list(data0, data1)))

})

test_that("ImageArrayList accessors", {

	data0 <- matrix(1:4, nrow=2)
	idata <- ImageArrayList(data0)
	expect_equal(idata[[1]], data0)

	data1 <- matrix(5:8, nrow=2)
	idata[["data1"]] <- data1
	expect_equal(idata[["data1"]], data1)
	
	data2 <- matrix(9:12, nrow=2)
	idata2 <- ImageArrayList(list(d0=data0, d1=data1, d2=data2))
	expect_equal(idata2[["d1"]], data1)
	
	expect_equal(names(idata2), c("d0", "d1", "d2"))
	expect_equal(length(idata2), 3)

})

test_that("ImageArrayList subsetting", {

	data0 <- matrix(1:9, nrow=3)
	data1 <- matrix(10:18, nrow=3)
	data2 <- matrix(19:27, nrow=3)
	idata <- ImageArrayList(list(d0=data0, d1=data1, d2=data2))
	
	expect_true(validObject(idata))

	idataS <- idata[1:2,1:2]
	expect_equal(idataS[[1]], data0[1:2,1:2])
	
	idataS <- idata[1:2,]
	expect_equal(idataS[[1]], data0[1:2,])

	idataS <- idata[,1:2]
	expect_equal(idataS[[1]], data0[,1:2])

})

test_that("ImageArrayList binding", {

	data0 <- matrix(1:9, nrow=3)
	data1 <- matrix(10:18, nrow=3)
	data2 <- matrix(19:27, nrow=3)
	idata <- ImageArrayList(list(d0=data0, d1=data1, d2=data2))
	
	expect_true(validObject(idata))

	idataB <- cbind(idata, idata)
	expect_equal(idataB[[1]], cbind(data0, data0))

	idataB <- rbind(idata, idata)
	expect_equal(idataB[[1]], rbind(data0, data0))

})
