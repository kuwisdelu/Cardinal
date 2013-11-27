require(testthat)

context("MSImageSpectra class")

test_that("MSImageSpectra validity", {
	
	expect_true(validObject(new("MSImageSpectra")))
	expect_true(validObject(MSImageSpectra()))

	spectra0 <- array(1:27, dim=c(3,3,3))
	msidata0 <- MSImageSpectra(spectra=spectra0)
	expect_true(validObject(msidata0))

	coord <- expand.grid(x=1:3, y=1:3)
	expect_warning(MSImageSpectra(spectra=spectra0, coord=coord), "spectra is a datacube")

	spectra1 <- matrix(1:27, nrow=3)
	msidata1 <- MSImageSpectra(spectra=spectra1)
	expect_true(validObject(msidata1))

})

test_that("MSImageSpectra manipulation", {

	spectra0 <- array(1:27, dim=c(3,3,3))
	msidata0 <- MSImageSpectra(spectra=spectra0)
	expect_equal(msidata0[], spectra0)
	
	expect_equal(msidata0[1,,], spectra0[1,,])
	expect_equal(msidata0[,1,], spectra0[,1,])
	expect_equal(msidata0[,,1], spectra0[,,1])

	expect_identical(dim(msidata0[1,1,,drop=TRUE]), NULL)
	expect_equal(dim(msidata0[1,1,,drop=FALSE]), c(1,1,3))

	spectra1 <- matrix(1:27, nrow=3)
	msidata1 <- MSImageSpectra(spectra=spectra1, storageMode="immutableEnvironment")
	expect_equal(spectra(msidata1), spectra1)

	coord1 <- expand.grid(x=1:3, y=1:3)
	msidata1 <- MSImageSpectra(spectra=spectra1, coord=coord1, storageMode="immutableEnvironment")
	expect_equal(msidata1[], spectra0)

	new.values <- 101:103
	spectra(msidata1)[,1] <- new.values
	expect_equal(spectra(msidata1)[,1], new.values)

	old.values <- new.values
	new.values <- 201:203
	msidata2 <- msidata1
	spectra(msidata2)[,1] <- new.values
	expect_equal(spectra(msidata2)[,1], new.values)
	expect_equal(spectra(msidata1)[,1], old.values)

	storageMode(msidata1) <- "lockedEnvironment"
	expect_error(spectra(msidata1)[,1] <- new.values)

	storageMode(msidata1) <- "immutableEnvironment"
	combdata <- combine(msidata1, msidata2)
	expect_true(all(spectra(combdata) == cbind(spectra(msidata1), spectra(msidata2))))

	multicombdata <- combine(msidata1, msidata2, msidata2)
	expect_true(all(spectra(multicombdata) == cbind(spectra(msidata1), spectra(msidata2), spectra(msidata2))))

})
