require(testthat)

context("OPLS")

test_that("Orthogonal partial least squares", {

	set.seed(1)
	data <- matrix(c(NA, NA, 1, 1, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA, 
		NA, NA, NA, NA, NA, 0, 1, 1, NA, NA, NA, NA, NA, 1, 0, 0, 1, 
		1, NA, NA, NA, NA, NA, 0, 1, 1, 1, 1, NA, NA, NA, NA, 0, 1, 1, 
		1, 1, 1, NA, NA, NA, NA, 1, 1, 1, 1, 1, 1, 1, NA, NA, NA, 1, 
		1, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA, NA, NA, NA), nrow=9, ncol=9)

	sset <- generateImage(data, range=c(1001, 5000), step=100)

	y <- factor(data[!is.na(data)], labels=c("black", "red"))

	opls <- OPLS(sset, y, ncomp=c(10,20))

	expect_equal(apply(opls$fitted[[1]], 1, which.max), as.integer(y))

})
