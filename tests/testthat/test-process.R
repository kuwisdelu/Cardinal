require(testthat)
require(Cardinal)

context("processing")

.setup_MSImagingArrays <- function(n = 10L, p = 20L)
{
	ns <- sample(p, n, replace=TRUE)
	mzrange <- runif(p, min=100, max=1000)
	intensity <- lapply(ns, function(ni) rlnorm(ni))
	mz <- lapply(ns, function(ni) sort(sample(mzrange, ni)))
	a <- SpectraArrays(list(mz=mz, intensity=intensity))
	MSImagingArrays(a, centroided=FALSE)
}

# tests for workhorse functions are currently in pkg:matter
# to be refactored and moved to pkg:CardinalCore eventually --
# we only check that the infrastructure works here

test_that("normalize", {

	path <- CardinalIO::exampleImzMLFile("processed")
	msa <- readImzML(path)
	mzref <- mz(msa)[[1L]][[1L]]

	msa_tic <- normalize(msa, method="tic")
	msa_rms <- normalize(msa, method="rms")
	msa_ref <- normalize(msa, method="reference", ref=mzref)

	expect_true(validObject(applyProcessing(msa_tic)))
	expect_true(validObject(applyProcessing(msa_rms)))
	expect_true(validObject(applyProcessing(msa_ref)))

})

test_that("smooth", {

	path <- CardinalIO::exampleImzMLFile("processed")
	msa <- readImzML(path)

	msa_gauss <- smooth(msa, method="gaussian")
	msa_bi <- smooth(msa, method="bilateral")
	msa_adapt <- smooth(msa, method="adaptive")
	msa_diff <- smooth(msa, method="diff")
	msa_guide <- smooth(msa, method="guide")
	msa_pag <- smooth(msa, method="pag")
	msa_sg <- smooth(msa, method="sgolay")
	msa_ma <- smooth(msa, method="ma")

	expect_true(validObject(applyProcessing(msa_gauss)))
	expect_true(validObject(applyProcessing(msa_bi)))
	expect_true(validObject(applyProcessing(msa_adapt)))
	expect_true(validObject(applyProcessing(msa_diff)))
	expect_true(validObject(applyProcessing(msa_guide)))
	expect_true(validObject(applyProcessing(msa_pag)))
	expect_true(validObject(applyProcessing(msa_sg)))
	expect_true(validObject(applyProcessing(msa_ma)))

})

test_that("reduceBaseline", {

	path <- CardinalIO::exampleImzMLFile("processed")
	msa <- readImzML(path)

	msa_locmin <- reduceBaseline(msa, method="locmin")
	msa_hull <- reduceBaseline(msa, method="hull")
	msa_snip <- reduceBaseline(msa, method="snip")
	msa_median <- reduceBaseline(msa, method="median", width=15)

	expect_true(validObject(applyProcessing(msa_locmin)))
	expect_true(validObject(applyProcessing(msa_hull)))
	expect_true(validObject(applyProcessing(msa_snip)))
	expect_true(validObject(applyProcessing(msa_median)))

})

test_that("recalibrate", {

	path <- CardinalIO::exampleImzMLFile("processed")
	msa <- readImzML(path)
	mzref <- estimateReferencePeaks(msa)

	msa_locmax <- recalibrate(msa, ref=mzref, method="locmax")
	# msa_dtw <- recalibrate(msa, ref=mzref, method="dtw")
	# msa_cow <- recalibrate(msa, ref=mzref, method="cow")

	expect_true(validObject(applyProcessing(msa_locmax)))
	# expect_true(validObject(applyProcessing(msa_dtw)))
	# expect_true(validObject(applyProcessing(msa_cow)))

})

test_that("peakPick", {

	path <- CardinalIO::exampleImzMLFile("processed")
	msa <- readImzML(path)
	mzref <- estimateReferencePeaks(msa)

	msa_peakpick <- peakPick(msa, method="diff")
	msa_peakpick_cwt <- peakPick(msa, method="cwt")
	msa_peakpick_ref <- peakPick(msa, ref=mzref)

	expect_true(validObject(applyProcessing(msa_peakpick)))
	expect_true(validObject(applyProcessing(msa_peakpick_cwt)))
	expect_true(validObject(applyProcessing(msa_peakpick_ref)))

})
