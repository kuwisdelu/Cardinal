require(testthat)

context("generatePositionArray")

test_that("generatePositionArray", {
	
	coord1 <- expand.grid(x=1:3, y=1:3)
	pos1 <- generatePositionArray(coord1)
	expect_equivalent(pos1, matrix(1:9, nrow=3))
	expect_identical(names(dim(pos1)), c("x", "y"))

	coord2 <- data.frame(x=1:3, y=1:3)
	pos2 <- generatePositionArray(coord2)
	x <- matrix(NA, nrow=3, ncol=3)
	diag(x) <- 1:3
	expect_equivalent(pos2, x)
	expect_identical(names(dim(pos2)), c("x", "y"))

})
