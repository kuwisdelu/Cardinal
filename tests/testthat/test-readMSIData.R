require(testthat)
require(Cardinal)

context("read/write imzML + Analyze 7.5")

test_that("read/write continuous", {

	path <- CardinalIO::exampleImzMLFile("continuous")
	mse <- readImzML(path)

	expect_true(is(mse, "MSImagingExperiment"))

	path2 <- paste0(tempfile(), ".imzML")
	writeImzML(mse, path2)
	mse2 <- readImzML(path2)

	expect_true(is(mse2, "MSImagingExperiment"))
	expect_equivalent(mz(mse), mz(mse2))
	expect_equivalent(spectra(mse)[,1L], spectra(mse2)[,1L])

	pData(mse2)$test <- seq_len(ncol(mse2))
	fData(mse2)$test <- seq_len(nrow(mse2))

	path3 <- paste0(tempfile(), ".imzML")
	writeImzML(mse2, path3)
	mse3 <- readImzML(path3)

	expect_equal(pData(mse2), pData(mse3))
	expect_equal(fData(mse2), fData(mse3))

	path4 <- paste0(tempfile(), ".img")
	writeAnalyze(mse, path4)
	mse4 <- readAnalyze(path4)

	expect_true(is(mse4, "MSImagingExperiment"))
	expect_equal(mz(mse), mz(mse4))
	expect_equal(spectra(mse)[,1L], spectra(mse4)[,1L])

})

test_that("read/write processed", {

	path <- CardinalIO::exampleImzMLFile("processed")
	msa <- readImzML(path)

	expect_true(is(msa, "MSImagingArrays"))

	path2 <- paste0(tempfile(), ".imzML")
	writeImzML(msa, path2)
	msa2 <- readImzML(path2)

	expect_true(is(msa2, "MSImagingArrays"))
	expect_equivalent(mz(msa)[[1L]], mz(msa2)[[1L]])
	expect_equivalent(intensity(msa)[[1L]], intensity(msa2)[[1L]])

	pData(msa2)$test <- seq_len(length(msa2))

	path3 <- paste0(tempfile(), ".imzML")
	writeImzML(msa2, path3)
	msa3 <- readImzML(path3)

	expect_equal(pData(msa2), pData(msa3))

})
