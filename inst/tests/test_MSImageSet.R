require(testthat)

context("MSImageSet class")

test_that("MSImageSet validity", {
	
	expect_true(validObject(new("MSImageSet")))
	expect_true(validObject(MSImageSet()))

	spectra0 <- array(1:27, dim=c(3,3,3))
	expect_warning(msset0 <- MSImageSet(spectra=spectra0))
	expect_true(validObject(msset0))

	spectra1 <- matrix(1:27, nrow=3)
	coord <- expand.grid(x=1:3, y=1:3)
	msset1 <- MSImageSet(spectra=spectra1, coord=coord)
	expect_true(validObject(msset1))

	mz <- c(100, 200, 300)
	msset2 <- MSImageSet(spectra=spectra1, mz=mz, coord=coord)
	expect_true(validObject(msset2))

})

test_that("MSImageSet imageData", {

	mz <- c(101, 102, 103)
	spectra <- matrix(1:27, nrow=3)
	coord <- expand.grid(x=1:3, y=1:3)
	msset <- MSImageSet(spectra=spectra, mz=mz, coord=coord)

	expect_identical(spectra(msset), spectra)
	
	dim(spectra) <- c(Features=3, x=3, y=3)
	expect_identical(imageData(msset)[], spectra)

	expect_identical(iData(msset), spectra(msset))

	msset2 <- msset
	spectra(msset2) <- matrix(27:1, nrow=3)
	expect_equal(sum(spectra(msset2) == spectra(msset)), 1)

})

test_that("MSImageSet pixelData", {

	mz <- c(101, 102, 103)
	spectra <- matrix(1:27, nrow=3)
	coord <- expand.grid(x=1:3, y=1:3)
	msset <- MSImageSet(spectra=spectra, mz=mz, coord=coord)

	msset[["test"]] <- rnorm(9)
	expect_identical(pData(msset)$test, msset$test)

	expect_equivalent(coord(msset), coord)

	# write tests for coord<- and positionArray updating

})
