require(testthat)
require(Cardinal)

context("process-spectra")

test_that("process spectra - SpectralImagingArrays", {

	set.seed(1, kind="default")
	x <- replicate(9, rlnorm(100), simplify=FALSE)
	t1 <- replicate(9, sort(runif(100)), simplify=FALSE)
	t2 <- replicate(9, sort(runif(100)), simplify=FALSE)
	t3 <- replicate(9, sort(runif(100)), simplify=FALSE)
	ones <- rep.int(1, length(x))

	s <- SpectralImagingArrays(
		spectraData=list(intensity=x, t1=t1, t2=t2, t3=t3),
		pixelData=PositionDataFrame(expand.grid(x=1:3, y=1:3)))

	s2 <- process(normalize(s, method="rms", scale=1))
	rms1 <- vapply(spectra(s2), \(.) sqrt(mean(.^2)), numeric(1L))
	
	expect_is(s2, "SpectralImagingArrays")
	expect_equal(rms1, ones)
	expect_equal(reset(normalize(s)), s)

	file1 <- tempfile()
	s3 <- normalize(s, method="rms", scale=1)
	s3 <- process(s3, outfile=file1)
	rms3 <- vapply(as.list(spectra(s3)), \(.) sqrt(mean(.^2)), numeric(1L))
	fout1 <- path(spectra(s3))

	expect_is(spectra(s3, "intensity"), "matter_list")
	expect_is(spectra(s3, "index"), "matter_list")
	expect_equal(normalizePath(fout1), normalizePath(file1))
	expect_equal(rms3, ones)

	s4 <- normalize(s, method="rms")
	s4 <- smooth(s4, method="adaptive")
	s4 <- reduceBaseline(s4, method="locmin")
	s4 <- process(s4)

	expect_is(s4, "SpectralImagingArrays")

	fun2 <- function(x, t1, t2) x / (t1 + t2)
	s5 <- addProcessing(s, fun2, "custom function 2d")
	s5 <- process(s5, index=c("t1", "t2"))

	expect_equal(spectra(s5), Map(fun2, x, t1, t2))

	fun3 <- function(x, t1, t2, t3) x / (t1 + t2 + t3)
	s6 <- addProcessing(s, fun3, "custom function 3d")
	s6 <- process(s6, index=c("t1", "t2", "t3"))

	expect_equal(spectra(s6), Map(fun3, x, t1, t2, t3))

	funout <- function(x, t1, t2) cbind(t1[1:3], t2[1:3], x[1:3])
	s7 <- addProcessing(s, funout, "custom function index output")
	s7 <- process(s7, index=c("t1", "t2"))

	expect_equal(spectra(s7), Map(\(.) .[1:3], x))
	expect_equal(spectra(s7, "t1"), Map(\(.) .[1:3], t1))
	expect_equal(spectra(s7, "t2"), Map(\(.) .[1:3], t2))

	file2 <- tempfile()
	s8 <- addProcessing(s, funout, "custom function index file output")
	s8 <- process(s8, index=c("t1", "t2"), outfile=file2)
	fout2 <- path(spectra(s8))

	expect_is(spectra(s8, "intensity"), "matter_list")
	expect_is(spectra(s8, "t1"), "matter_list")
	expect_is(spectra(s8, "t2"), "matter_list")
	expect_equal(normalizePath(fout2), normalizePath(file2))
	expect_equal(spectra(s8, "intensity")[[1L]], x[[1L]][1:3])
	expect_equal(spectra(s8, "t1")[[1L]], t1[[1L]][1:3])
	expect_equal(spectra(s8, "t2")[[1L]], t2[[1L]][1:3])

})

test_that("process spectra - SpectralImagingExperiment", {

	set.seed(1, kind="default")
	x <- replicate(9, rlnorm(100))
	t1 <- sort(runif(100))
	t2 <- sort(runif(100))
	ones <- rep.int(1, ncol(x))

	s <- SpectralImagingExperiment(
		spectraData=list(intensity=x),
		featureData=DataFrame(t1=t1, t2=t2),
		pixelData=PositionDataFrame(expand.grid(x=1:3, y=1:3)))

	s2 <- process(normalize(s, method="rms", scale=1))

	expect_is(s2, "SpectralImagingExperiment")
	expect_equal(sqrt(colMeans(spectra(s2)^2)), ones)
	expect_equal(reset(normalize(s)), s)

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
	rms4 <- vapply(as.list(intensity(ms3)), \(.) sqrt(mean(.^2)), numeric(1L))
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

	ref <- summarizeFeatures(as(ms, "MSImagingExperiment"))
	peaks <- matter::findpeaks(fData(ref)$mean, snr=3)
	peaks <- mz(ref)[peaks]
	ms5 <- recalibrate(ms, ref=peaks, tolerance=200)
	ms5 <- process(ms5)

	expect_is(ms5, "MSImagingArrays")

	peaks2 <- estimateReferencePeaks(ms)
	ms6 <- recalibrate(ms, ref=peaks2, tolerance=200)
	ms6 <- process(ms6)

	expect_is(ms6, "MSImagingArrays")

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

	peaks2 <- estimateReferencePeaks(ms)
	ms6 <- recalibrate(ms, ref=peaks2, tolerance=200)
	ms6 <- process(ms6)

	expect_is(ms6, "MSImagingExperiment")

})
