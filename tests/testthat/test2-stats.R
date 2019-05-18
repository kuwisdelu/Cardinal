require(testthat)
require(Cardinal)

context("stats-2")

options(Cardinal.progress=FALSE, Cardinal.verbose=FALSE)

register(SerialParam())

set.seed(1)
x <- simulateImage(preset=2, npeaks=10, dim=c(10,10),
	peakheight=c(2,4), representation="centroid")

y <- makeFactor(circle=pData(x)$circle, square=pData(x)$square)

set.seed(1)
testdata <- simulateImage(preset=4, npeaks=10, nruns=3,
	dim=c(10,10), sdnoise=0.5, peakheight=2,
	peakdiff=2, representation="centroid")

test_that("PCA", {

	res1 <- PCA(x, ncomp=1:3)

	expect_true(validObject(res1))

	expect_is(summary(res1), "SummaryPCA")

})

test_that("PLS", {

	res1 <- PLS(x, y, ncomp=1:3)

	expect_true(validObject(res1))

	expect_is(summary(res1), "SummaryPLS")

})

test_that("OPLS", {

	res1 <- OPLS(x, y, ncomp=1:3)

	expect_true(validObject(res1))

	expect_is(summary(res1), "SummaryPLS")

})

test_that("spatialFastmap", {

	res1 <- spatialFastmap(x, r=c(1,2), ncomp=2, method="gaussian")

	expect_true(validObject(res1))

	expect_is(summary(res1), "SummarySpatialFastmap")

	res2 <- spatialFastmap(x, r=c(1,2), ncomp=2, method="adaptive")

	expect_true(validObject(res2))

	expect_is(summary(res2), "SummarySpatialFastmap")

})

test_that("spatialKMeans", {

	set.seed(1)
	res1 <- spatialKMeans(x, r=c(1,2), k=c(2,3), method="gaussian")

	expect_true(validObject(res1))

	expect_is(summary(res1), "SummarySpatialKMeans")

	set.seed(1)
	res2 <- spatialKMeans(x, r=c(1,2), k=c(2,3), method="adaptive")

	expect_true(validObject(res2))

	expect_is(summary(res2), "SummarySpatialKMeans")

})

test_that("spatialShrunkenCentroids", {

	set.seed(1)
	res1 <- spatialShrunkenCentroids(x, r=c(1,2), k=3, s=c(0,3,6), method="gaussian")

	expect_true(validObject(res1))

	expect_is(summary(res1), "SummarySpatialShrunkenCentroids")

	set.seed(1)
	res2 <- spatialShrunkenCentroids(x, r=c(1,2), k=3, s=c(0,3,6), method="adaptive")

	expect_true(validObject(res2))

	expect_is(summary(res2), "SummarySpatialShrunkenCentroids")

	res3 <- spatialShrunkenCentroids(x, y, r=c(1,2), s=c(0,3,6), method="gaussian")

	expect_true(validObject(res3))

	expect_is(summary(res3), "SummarySpatialShrunkenCentroids")

	res4 <- spatialShrunkenCentroids(x, y, r=c(1,2), s=c(0,3,6), method="adaptive")

	expect_true(validObject(res4))

	expect_is(summary(res4), "SummarySpatialShrunkenCentroids")

})

test_that("spatialDGMM", {

	set.seed(1)
	res1 <- spatialDGMM(x, r=1, k=3, method="gaussian")

	expect_true(validObject(res1))

	expect_is(summary(res1), "SummarySpatialDGMM")

	set.seed(1)
	res2 <- spatialDGMM(x, r=1, k=3, method="adaptive")

	expect_true(validObject(res2))

	expect_is(summary(res2), "SummarySpatialDGMM")

})

test_that("meansTest + segmentationTest", {

	res1 <- meansTest(testdata, ~ condition)

	expect_true(validObject(res1))

	expect_is(summary(res1), "SummaryMeansTest")

	set.seed(1)
	res2 <- segmentationTest(testdata, ~ condition, classControl="Ymax")

	expect_true(validObject(res2))

	expect_is(summary(res2), "SummarySegmentationTest")

})

