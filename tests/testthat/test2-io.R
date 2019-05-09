require(testthat)
require(Cardinal)

context("I/O")

options(Cardinal.progress=FALSE, Cardinal.verbose=FALSE)

register(SerialParam())

set.seed(1)
data <- simulateImage(preset=1, dim=c(3,3), representation="centroid")

set.seed(1)
data3d <- simulateImage(preset=9, nruns=3, dim=c(3,3), representation="centroid")
run(data3d) <- "run0"

test_that("write/read Analyze", {

	file1 <- tempfile()

	out1 <- writeMSIData(data, file=file1, outformat="Analyze")

	expect_true(out1)

	in1 <- readAnalyze(name=basename(file1), folder=dirname(file1))

	expect_true(validObject(in1))

	expect_equal(dim(data), dim(in1))

	file2 <- tempfile()

	out2 <- writeMSIData(data3d, file=file2, outformat="Analyze")

	expect_true(out2)

	in2 <- readAnalyze(name=basename(file2), folder=dirname(file2))

	expect_true(validObject(in2))

	expect_equal(dim(data3d), dim(in2))

})

test_that("write/read imzML", {

	file1 <- tempfile()

	out1 <- writeMSIData(data, file=file1, outformat="imzML")

	expect_true(out1)

	in1 <- readImzML(name=basename(file1), folder=dirname(file1))

	expect_true(validObject(in1))

	expect_equal(dim(data), dim(in1))

	file2 <- tempfile()

	out2 <- writeMSIData(data3d, file=file2, outformat="imzML")

	expect_true(out2)

	in2 <- readImzML(name=basename(file2), folder=dirname(file2))

	expect_true(validObject(in2))

	expect_equal(dim(data3d), dim(in2))

})

