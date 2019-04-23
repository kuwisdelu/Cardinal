require(testthat)
require(Cardinal)

context("apply")

options(Cardinal.progress=FALSE, Cardinal.verbose=FALSE)

register(SerialParam())

set.seed(1)
x <- simulateImage(preset=7, nruns=3, npeaks=10, dim=c(10,10),
	peakheight=2, peakdiff=2, representation="centroid")

y <- makeFactor(A=pData(x)$circleA, B=pData(x)$circleB)

test_that("featureApply", {

	out1 <- featureApply(x, mean)
	out2 <- apply(iData(x), 1, mean)

	expect_equal(out1, out2)

	out3 <- featureApply(x, mean, .outpath=tempfile())

	expect_equal(out1, out3[])

})

test_that("pixelApply", {

	out1 <- pixelApply(x, sum)
	out2 <- apply(iData(x), 2, sum)

	expect_equal(out1, out2)

	out3 <- pixelApply(x, sum, .outpath=tempfile())

	expect_equal(out1, out3[])

})

test_that("cvApply", {

	out1 <- cvApply(x, y, s=c(0,3,6), .fun=spatialShrunkenCentroids)

	acc1 <- rowMeans(sapply(out1, function(o) summary(o)$Accuracy))

	out2 <- crossValidate(x, y, s=c(0,3,6), .fun=spatialShrunkenCentroids)

	acc2 <- summary(out2)$Accuracy

	expect_true(validObject(out2))

	expect_equal(acc1, acc2)

	out3 <- crossValidate(x, y, ncomp=1:5, .fun=PLS)

	expect_true(validObject(out3))

	out4 <- crossValidate(x, y, ncomp=1:5, .fun=OPLS)

	expect_true(validObject(out4))

})
