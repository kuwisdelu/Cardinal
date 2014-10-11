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
	sdata1 <- MSImageData(data=data1)
	expect_true(validObject(sdata1))

})
