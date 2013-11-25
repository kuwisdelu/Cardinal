context("MSImageSpectra class")

test_that("MSImageSpectra validity", {
	
	expect_true(validObject(new("MSImageSpectra")))

	spectra0 <- array(1:27, dim=c(3,3,3))
	msidata0 <- MSImageSpectra(spectra=spectra0)
	expect_that(validObject(msidata0), is_true())

	coord <- expand.grid(x=1:3, y=1:3)
	expect_that(MSImageSpectra(spectra=spectra0, coord=coord), gives_warning("spectra is a datacube"))

	spectra1 <- matrix(1:27, nrow=3)
	msidata1 <- MSImageSpectra(spectra=spectra1)
	expect_that(validObject(msidata1), is_true())

})

test_that("MSImageSpectra manipulation", {

	spectra0 <- array(1:27, dim=c(3,3,3))
	msidata0 <- MSImageSpectra(spectra=spectra0)
	expect_that(msidata0[], equals(spectra0))
	
	expect_that(msidata0[1,,], equals(spectra0[1,,]))
	expect_that(msidata0[,1,], equals(spectra0[,1,]))
	expect_that(msidata0[,,1], equals(spectra0[,,1]))

	expect_that(dim(msidata0[1,1,,drop=TRUE]), is_identical_to(NULL))
	expect_that(dim(msidata0[1,1,,drop=FALSE]), equals(c(1,1,3)))

	spectra1 <- matrix(1:27, nrow=3)
	msidata1 <- MSImageSpectra(spectra=spectra1, storageMode="immutableEnvironment")
	expect_that(spectra(msidata1), equals(spectra1))

	new.values <- 101:103
	spectra(msidata1)[,1] <- new.values
	expect_that(spectra(msidata1)[,1], equals(new.values))

	old.values <- new.values
	new.values <- 201:203
	msidata2 <- msidata1
	spectra(msidata2)[,1] <- new.values
	expect_that(spectra(msidata2)[,1], equals(new.values))
	expect_that(spectra(msidata1)[,1], equals(old.values))

	storageMode(msidata1) <- "lockedEnvironment"
	expect_that(spectra(msidata1)[,1] <- new.values, throws_error())

})
