require(testthat)
require(Cardinal)

context("process-bin")

test_that("process bin - SpectralImagingArrays", {

	path <- CardinalIO::exampleImzMLFile("processed")
	s <- as(readImzML(path), "SpectralImagingArrays")

	s2 <- bin(s, units="relative")
	s3 <- bin(s, units="absolute")

	expect_is(s2, "SpectralImagingExperiment")
	expect_is(s3, "SpectralImagingExperiment")
	expect_is(spectra(s2), "sparse_mat")
	expect_is(spectra(s3), "sparse_mat")
	expect_equal(fData(s3)$index, seq_len(8399))

	s4 <- bin(s, ref=fData(s3)$index, units="absolute")
	s5 <- bin(s, ref=c(100, 500), resolution=1, units="absolute")

	expect_equal(s4, s3)
	expect_equal(range(fData(s5)$index), c(100, 500))
	expect_equal(estres(fData(s5)$index), c(absolute=1))

})

test_that("process bin - SpectralImagingExperiment", {

	path <- CardinalIO::exampleImzMLFile("continuous")
	s <- as(readImzML(path), "SpectralImagingExperiment")

	s2 <- bin(s, units="relative")
	s3 <- bin(s, units="absolute")

	expect_is(s2, "SpectralImagingExperiment")
	expect_is(s3, "SpectralImagingExperiment")
	expect_is(spectra(s2), "sparse_mat")
	expect_is(spectra(s3), "sparse_mat")
	expect_equal(fData(s3)$index, seq_len(8399))

	s4 <- bin(s, ref=fData(s3)$index, units="absolute")
	s5 <- bin(s, ref=c(100, 500), resolution=1, units="absolute")

	expect_equal(s4, s3)
	expect_equal(range(fData(s5)$index), c(100, 500))
	expect_equal(estres(fData(s5)$index), c(absolute=1))
	
})

test_that("process bin - MSImagingArrays", {

	path <- CardinalIO::exampleImzMLFile("processed")
	ms <- readImzML(path)

	ms2 <- bin(ms, units="ppm")
	ms3 <- bin(ms, units="mz")

	expect_is(ms2, "MSImagingExperiment")
	expect_is(ms3, "MSImagingExperiment")
	expect_is(spectra(ms2), "sparse_mat")
	expect_is(spectra(ms3), "sparse_mat")

})

test_that("process bin - MSImagingExperiment", {

	path <- CardinalIO::exampleImzMLFile("continuous")
	ms <- readImzML(path)

	ms2 <- bin(ms, units="ppm")
	ms3 <- bin(ms, units="mz")

	expect_is(ms2, "MSImagingExperiment")
	expect_is(ms3, "MSImagingExperiment")
	expect_is(spectra(ms2), "sparse_mat")
	expect_is(spectra(ms3), "sparse_mat")

})
