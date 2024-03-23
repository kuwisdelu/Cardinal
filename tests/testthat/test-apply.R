require(testthat)
require(Cardinal)

context("spectrapply")

test_that("spectrapply", {

	path <- CardinalIO::exampleImzMLFile("continuous")
	mse <- readImzML(path, memory=TRUE)

	xout <- spectrapply(mse, function(x, t, ...) x)
	mzout <- spectrapply(mse, function(x, t, ...) t)

	expect_equal(spectra(mse), xout)
	expect_equal(mz(mse), mzout[,1L])

	path2 <- CardinalIO::exampleImzMLFile("processed")
	mse2 <- readImzML(path2, memory=TRUE)

	xout2 <- spectrapply(mse2, function(x, t, ...) x, simplify=FALSE)
	mzout2 <- spectrapply(mse2, function(x, t, ...) t, simplify=FALSE)

	expect_equal(intensity(mse2), xout2)
	expect_equal(mz(mse2), mzout2)

})

