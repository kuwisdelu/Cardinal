require(testthat)

context("PLS")

test_that("Partial least squares", {

	options(Cardinal.progress=FALSE, Cardinal.verbose=FALSE)

	set.seed(1)
	data <- matrix(c(NA, NA, 1, 1, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA, 
		NA, NA, NA, NA, NA, 0, 1, 1, NA, NA, NA, NA, NA, 1, 0, 0, 1, 
		1, NA, NA, NA, NA, NA, 0, 1, 1, 1, 1, NA, NA, NA, NA, 0, 1, 1, 
		1, 1, 1, NA, NA, NA, NA, 1, 1, 1, 1, 1, 1, 1, NA, NA, NA, 1, 
		1, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA, NA, NA, NA), nrow=9, ncol=9)

	sset <- generateImage(data, range=c(200, 300), step=1)

	y <- factor(data[!is.na(data)], labels=c("black", "red"))

	pls <- PLS(sset, y, ncomp=c(10,20))

	expect_equal(apply(pls$fitted[[1]], 1, which.max), as.integer(y))

	expect_error(PLS(sset, y, ncomp=1000))

})
