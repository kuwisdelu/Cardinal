require(testthat)
require(Cardinal)

context("process")

test_that("process spectra - SpectralImagingArrays", {

	path <- CardinalIO::exampleImzMLFile("processed")
	s <- as(readImzML(path), "SpectralImagingArrays")
	ones <- rep.int(1, length(s))

	s2 <- process(normalize(s, method="rms", scale=1))
	rms1 <- vapply(spectra(s2, 2L), \(.) sqrt(mean(.^2)), numeric(1L))
	
	expect_is(s2, "SpectralImagingArrays")
	expect_equal(rms1, ones)

	file1 <- tempfile()
	s3 <- normalize(s, method="rms", scale=1)
	s3 <- process(s3, outfile=file1)
	rms3 <- vapply(as.list(spectra(s3, 2L)), \(.) sqrt(mean(.^2)), numeric(1L))
	fout1 <- path(spectra(s3, 2L))

	expect_is(spectra(s3, 1L), "matter_list")
	expect_is(spectra(s3, 2L), "matter_list")
	expect_equal(normalizePath(fout1), normalizePath(file1))
	expect_equal(rms3, ones)

	s4 <- normalize(s, method="rms")
	s4 <- smooth(s4, method="adaptive")
	s4 <- reduceBaseline(s4, method="locmin")
	s4 <- process(s4)

	expect_is(s4, "SpectralImagingArrays")

})

test_that("process spectra - SpectralImagingExperiment", {

	path <- CardinalIO::exampleImzMLFile("continuous")
	s <- as(readImzML(path), "SpectralImagingExperiment")
	ones <- rep.int(1, length(s))

	s2 <- process(normalize(s, method="rms", scale=1))

	expect_is(s2, "SpectralImagingExperiment")
	expect_equal(sqrt(colMeans(spectra(s2)^2)), ones)

	file1 <- tempfile()
	s3 <- normalize(s, method="rms", scale=1)
	s3 <- process(s3, outfile=file1)
	fout1 <- path(spectra(s3))

	expect_is(spectra(s3), "matter_mat")
	expect_equal(normalizePath(fout1), normalizePath(file1))
	expect_equal(sqrt(colMeans(as.matrix(spectra(s3))^2)), ones)

	s4 <- normalize(s, method="rms")
	s4 <- smooth(s4, method="adaptive")
	s4 <- reduceBaseline(s4, method="locmin")
	s4 <- process(s4)

	expect_is(s4, "SpectralImagingExperiment")

})

test_that("process spectra - MSImagingArrays", {

	path <- CardinalIO::exampleImzMLFile("processed")
	ms <- readImzML(path)
	ones <- rep.int(1, length(ms))

	ms2 <- process(normalize(ms, method="rms", scale=1))
	rms2 <- vapply(intensity(ms2), \(.) sqrt(mean(.^2)), numeric(1L))

	expect_is(ms2, "MSImagingArrays")
	expect_equal(rms2, ones)

	file2 <- paste0(tempfile(), ".imzML")
	ms3 <- normalize(ms, method="rms", scale=1)
	ms3 <- process(ms3, outfile=file2)
	rms4 <- vapply(as.list(spectra(ms3, 2L)), \(.) sqrt(mean(.^2)), numeric(1L))
	fout2 <- path(intensity(ms3))

	expect_is(mz(ms3), "matter_list")
	expect_is(intensity(ms3), "matter_list")
	expect_equal(normalizePath(dirname(fout2)), normalizePath(file2))
	expect_equal(rms4, ones)

	ms4 <- normalize(ms, method="rms")
	ms4 <- smooth(ms4, method="adaptive")
	ms4 <- reduceBaseline(ms4, method="locmin")
	ms4 <- process(ms4)

	expect_is(ms4, "MSImagingArrays")

	ref <- summarizeFeatures(ms)
	peaks <- matter::findpeaks(fData(ref)$mean, snr=3)
	peaks <- mz(ref)[peaks]
	ms5 <- recalibrate(ms, ref=peaks, tolerance=200)
	ms5 <- process(ms5)

	expect_is(ms5, "MSImagingArrays")

})

test_that("process spectra - MSImagingExperiment", {

	path <- CardinalIO::exampleImzMLFile("continuous")
	ms <- readImzML(path)
	ones <- rep.int(1, length(ms))

	ms2 <- process(normalize(ms, method="rms", scale=1))

	expect_is(ms2, "MSImagingExperiment")
	expect_equal(sqrt(colMeans(spectra(ms2)^2)), ones)

	file2 <- paste0(tempfile(), ".imzML")
	ms3 <- normalize(ms, method="rms", scale=1)
	ms3 <- process(ms3, outfile=file2)
	fout2 <- path(spectra(ms3))

	expect_is(spectra(ms3), "matter_mat")
	expect_equal(normalizePath(dirname(fout2)), normalizePath(file2))
	expect_equal(sqrt(colMeans(as.matrix(spectra(ms3))^2)), ones)

	ms4 <- normalize(ms, method="rms")
	ms4 <- smooth(ms4, method="adaptive")
	ms4 <- reduceBaseline(ms4, method="locmin")
	ms4 <- process(ms4)

	expect_is(ms4, "MSImagingExperiment")

	ref <- summarizeFeatures(ms)
	peaks <- matter::findpeaks(fData(ref)$mean, snr=3)
	peaks <- mz(ref)[peaks]
	ms5 <- recalibrate(ms, ref=peaks, tolerance=200)
	ms5 <- process(ms5)

	expect_is(ms5, "MSImagingExperiment")

})
