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
	expect_equal(matter::estres(fData(s5)$index), c(absolute=1))

	s6 <- bin(s, ref=c(100, 500), tolerance=1, units="absolute")

	expect_equal(fData(s6)$index, c(100, 500))
	expect_equivalent(tolerance(spectra(s6)), 1)

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
	expect_equal(matter::estres(fData(s5)$index), c(absolute=1))

	s6 <- bin(s, ref=c(100, 500), tolerance=1, units="absolute")

	expect_equal(fData(s6)$index, c(100, 500))
	expect_equivalent(tolerance(spectra(s6)), 1)
	
})

test_that("process bin - MSImagingArrays", {

	path <- CardinalIO::exampleImzMLFile("processed")
	ms <- readImzML(path)

	ms2 <- bin(ms, units="ppm")
	ms3 <- bin(ms, units="mz")
	ms4 <- bin(ms, resolution=1, units="mz")
	ms5 <- bin(ms, mass.range=c(200, 600), resolution=1, units="mz")
	ms6 <- bin(ms, ref=152.9, tolerance=50, units="ppm")

	expect_is(ms2, "MSImagingExperiment")
	expect_is(ms3, "MSImagingExperiment")
	expect_is(spectra(ms2), "sparse_mat")
	expect_is(spectra(ms3), "sparse_mat")
	expect_equal(mz(ms4), 100:800)
	expect_equal(mz(ms5), 200:600)
	expect_equal(mz(ms6), 152.9)
	expect_equivalent(tolerance(spectra(ms6)), 5e-5)

	expect_error(bin(ms, mass.range=c(200, 600)))

})

test_that("process bin - MSImagingExperiment", {

	path <- CardinalIO::exampleImzMLFile("continuous")
	ms <- readImzML(path)

	ms2 <- bin(ms, units="ppm")
	ms3 <- bin(ms, units="mz")
	ms4 <- bin(ms, resolution=1, units="mz")
	ms5 <- bin(ms, mass.range=c(200, 600), resolution=1, units="mz")
	ms6 <- bin(ms, ref=152.9, tolerance=50, units="ppm")

	expect_is(ms2, "MSImagingExperiment")
	expect_is(ms3, "MSImagingExperiment")
	expect_is(spectra(ms2), "sparse_mat")
	expect_is(spectra(ms3), "sparse_mat")
	expect_equal(mz(ms4), 100:800)
	expect_equal(mz(ms5), 200:600)
	expect_equal(mz(ms6), 152.9)
	expect_equivalent(tolerance(spectra(ms6)), 5e-5)

	expect_error(bin(ms, mass.range=c(200, 600)))

})
