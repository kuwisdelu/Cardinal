require(testthat)

context("SpatialKMeans")

test_that("Spatially-aware k-means", {

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
	gkmeans <- spatialKMeans(sset, r=c(1,2), k=c(2,3), method="gaussian")

	expect_true(validObject(gkmeans))

	set.seed(1)
	akmeans <- spatialKMeans(sset, r=c(1,2), k=c(2,3), method="adaptive")

	expect_true(validObject(akmeans))

})
