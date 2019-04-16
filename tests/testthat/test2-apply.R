require(testthat)
require(Cardinal)

context("apply")

options(Cardinal.progress=FALSE, Cardinal.verbose=FALSE)

register(SerialParam())

set.seed(1)
x <- simulateImage(preset=2, nruns=3, npeaks=10, dim=c(10,10),
	peakheight=3, peakdiff=1, representation="centroid")

y <- makeFactor(circle=pData(x)$circle, square=pData(x)$square)

test_that("featureApply", {

	out1 <- featureApply(x, mean)
	out2 <- apply(iData(x), 1, mean)

	expect_equal(out1, out2)

})

test_that("pixelApply", {

	out1 <- pixelApply(x, mean)
	out2 <- apply(iData(x), 2, mean)

	expect_equal(out1, out2)

})

test_that("cvApply", {

	out <- cvApply(x, y, .fun=spatialShrunkenCentroids)

	expect_equal(out1, out2)

})
