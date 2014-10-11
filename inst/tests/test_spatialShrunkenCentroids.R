require(testthat)

context("SpatialShrunkenCentroids")

test_that("Spatially-aware shrunken centroids", {

	options(Cardinal.progress=FALSE, Cardinal.verbose=FALSE)

	set.seed(1)
	data <- matrix(c(NA, NA, 1, 1, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA, 
		NA, NA, NA, NA, NA, 0, 1, 1, NA, NA, NA, NA, NA, 1, 0, 0, 1, 
		1, NA, NA, NA, NA, NA, 0, 1, 1, 1, 1, NA, NA, NA, NA, 0, 1, 1, 
		1, 1, 1, NA, NA, NA, NA, 1, 1, 1, 1, 1, 1, 1, NA, NA, NA, 1, 
		1, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA, NA, NA, NA), nrow=9, ncol=9)

	sset <- generateImage(data, range=c(200, 300), step=1)

	y <- factor(data[!is.na(data)], labels=c("black", "red"))

	set.seed(1)
	gcentroids <- spatialShrunkenCentroids(sset, r=c(1,2), k=c(2,3), s=c(0,1), method="gaussian")

	expect_true(validObject(gcentroids))

	set.seed(1)
	acentroids <- spatialShrunkenCentroids(sset, r=c(1,2), k=c(2,3), s=c(0,1), method="adaptive")

	expect_true(validObject(acentroids))

	gfit <- spatialShrunkenCentroids(sset, y, r=c(1,2), s=c(0,1), method="gaussian")

	expect_true(validObject(gfit))

	afit <- spatialShrunkenCentroids(sset, y, r=c(1,2), s=c(0,1), method="adaptive")

	expect_true(validObject(afit))

})
