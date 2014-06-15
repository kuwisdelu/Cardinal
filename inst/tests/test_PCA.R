require(testthat)

context("PCA")

test_that("Principal components analysis", {

	set.seed(1)
	data <- matrix(c(NA, NA, 1, 1, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA, 
		NA, NA, NA, NA, NA, 0, 1, 1, NA, NA, NA, NA, NA, 1, 0, 0, 1, 
		1, NA, NA, NA, NA, NA, 0, 1, 1, 1, 1, NA, NA, NA, NA, 0, 1, 1, 
		1, 1, 1, NA, NA, NA, NA, 1, 1, 1, 1, 1, 1, 1, NA, NA, NA, 1, 
		1, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA, NA, NA, NA), nrow=9, ncol=9)

	sset <- generateImage(data, range=c(1001, 5000), step=100)

	pca <- PCA(sset, ncomp=c(10,20))

	prcomp <- prcomp(t(iData(sset)))$rotation[,1:10]

	dimnames(prcomp) <- dimnames(pca$loadings[[1]])

	expect_equal(abs(pca$loadings[[1]]), abs(prcomp), tolerance=0.1)

})
