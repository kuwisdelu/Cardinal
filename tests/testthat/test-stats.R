require(testthat)
require(Cardinal)

context("stats")

# options(Cardinal.progress=FALSE, Cardinal.verbose=FALSE)

# set.seed(1)
# data <- matrix(c(NA, NA, 1, 1, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA, 
# 	NA, NA, NA, NA, NA, 0, 1, 1, NA, NA, NA, NA, NA, 1, 0, 0, 1, 
# 	1, NA, NA, NA, NA, NA, 0, 1, 1, 1, 1, NA, NA, NA, NA, 0, 1, 1, 
# 	1, 1, 1, NA, NA, NA, NA, 1, 1, 1, 1, 1, 1, 1, NA, NA, NA, 1, 
# 	1, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA, NA, NA, NA), nrow=9, ncol=9)

# sset <- generateImage(data, range=c(200, 300), step=1)
# y <- factor(data[!is.na(data)], labels=c("black", "red"))

test_that("PCA", {

	# pca <- suppressWarnings(PCA(sset, ncomp=c(10,20))) # irlba suggests svd
	# prcomp <- prcomp(t(iData(sset)))$rotation[,1:10]
	# dimnames(prcomp) <- dimnames(pca$loadings[[1]])

	# expect_equal(abs(pca$loadings[[1]]), abs(prcomp), tolerance=0.1)
	# expect_error(PCA(sset, ncomp=1000))

})

test_that("PLS", {

	# pls <- PLS(sset, y, ncomp=c(10,20))

	# expect_equal(apply(pls$fitted[[1]], 1, which.max), as.integer(y))
	# expect_error(PLS(sset, y, ncomp=1000))

})

test_that("OPLS", {

	# opls <- OPLS(sset, y, ncomp=c(10,20))

	# expect_equal(apply(opls$fitted[[1]], 1, which.max), as.integer(y))
	# expect_error(OPLS(sset, y, ncomp=1000))

})

test_that("spatialFastmap", {

	# gfmap <- spatialFastmap(sset, r=c(1,2), ncomp=10, method="gaussian")

	# expect_true(validObject(gfmap))

	# afmap <- spatialFastmap(sset, r=c(1,2), ncomp=10, method="adaptive")

	# expect_true(validObject(afmap))

})

test_that("spatialKMeans", {

	# set.seed(1)
	# gkmeans <- spatialKMeans(sset, r=c(1,2), k=c(2,3), method="gaussian")

	# expect_true(validObject(gkmeans))

	# set.seed(1)
	# akmeans <- spatialKMeans(sset, r=c(1,2), k=c(2,3), method="adaptive")

	# expect_true(validObject(akmeans))

})

test_that("spatialShrunkenCentroids", {

	# set.seed(1)
	# gcentroids <- spatialShrunkenCentroids(sset, r=c(1,2), k=c(2,3), s=c(0,1), method="gaussian")

	# expect_true(validObject(gcentroids))

	# set.seed(1)
	# acentroids <- spatialShrunkenCentroids(sset, r=c(1,2), k=c(2,3), s=c(0,1), method="adaptive")

	# expect_true(validObject(acentroids))

	# gfit <- spatialShrunkenCentroids(sset, y, r=c(1,2), s=c(0,1), method="gaussian")

	# expect_true(validObject(gfit))

	# afit <- spatialShrunkenCentroids(sset, y, r=c(1,2), s=c(0,1), method="adaptive")

	# expect_true(validObject(afit))

})

