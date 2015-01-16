require(testthat)

context("MSImageData class")

test_that("MSImageData validity", {
	
	expect_true(validObject(new("MSImageData")))
	expect_true(validObject(MSImageData()))

	data0 <- array(1:27, dim=c(3,3,3))
	expect_true(validObject(MSImageData(data=data0)))

	coord <- expand.grid(x=1:3, y=1:3)
	expect_true(validObject(MSImageData(data=data0, coord=coord)))

	data1 <- matrix(1:27, nrow=3)
	msdata1 <- MSImageData(data=data1)
	expect_true(validObject(msdata1))

})

test_that("MSImageData combine", {

	data1 <- matrix(1:27, nrow=3)
	msdata1 <- MSImageData(data=data1)
	msdata2 <- msdata1

	coord(msdata1)$sample <- factor(1)
	coord(msdata2)$sample <- factor(2)

	msdata3 <- combine(msdata1, msdata2)

	expect_true(validObject(msdata3))

	expect_equivalent(iData(msdata3), cbind(iData(msdata1), iData(msdata2)))

})
