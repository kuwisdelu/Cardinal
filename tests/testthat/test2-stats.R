require(testthat)
require(Cardinal)

context("stats 2")

options(Cardinal.progress=FALSE, Cardinal.verbose=FALSE)

register(SerialParam())

set.seed(1)
x <- simulateImage(preset=2, npeaks=10, dim=c(10,10),
	peakheight=3, peakdiff=1, representation="centroid")

y <- makeFactor(circle=pData(x)$circle, square=pData(x)$square)

test_that("spatialFastmap", {

	res1 <- spatialFastmap(x, r=c(1,2), ncomp=2, method="gaussian")

	expect_true(validObject(res1))

	res2 <- spatialFastmap(x, r=c(1,2), ncomp=2, method="adaptive")

	expect_true(validObject(res2))

})

test_that("spatialKMeans", {

	set.seed(1)
	res1 <- spatialKMeans(x, r=c(1,2), k=c(2,3), method="gaussian")

	expect_true(validObject(res1))

	set.seed(1)
	res2 <- spatialKMeans(x, r=c(1,2), k=c(2,3), method="adaptive")

	expect_true(validObject(res2))

})

test_that("spatialShrunkenCentroids", {

	set.seed(1)
	res1 <- spatialShrunkenCentroids(x, r=c(1,2), k=c(2,3), s=c(0,6), method="gaussian")

	expect_true(validObject(res1))

	set.seed(1)
	res2 <- spatialShrunkenCentroids(x, r=c(1,2), k=c(2,3), s=c(0,6), method="adaptive")

	expect_true(validObject(res2))

	res3 <- spatialShrunkenCentroids(x, y, r=c(1,2), s=c(0,6), method="gaussian")

	expect_true(validObject(res3))

	res4 <- spatialShrunkenCentroids(x, y, r=c(1,2), s=c(0,6), method="adaptive")

	expect_true(validObject(res4))

})

test_that("spatialDGMM", {

	set.seed(1)
	res1 <- spatialDGMM(x, r=1, k=3, method="gaussian")

	expect_true(validObject(res1))

	set.seed(1)
	res2 <- spatialDGMM(x, r=1, k=3, method="adaptive")

	expect_true(validObject(res2))

})
