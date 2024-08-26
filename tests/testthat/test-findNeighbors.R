require(testthat)
require(Cardinal)

context("findNeighbors")

test_that("findNeighbors", {

	pos <- expand.grid(x=1:9, y=1:9)
	pos <- PositionDataFrame(coord=pos)

	nb <- findNeighbors(pos, r=1)
	nb2 <- findNeighbors(pos, r=1, metric="euclidean")
	nb3 <- findNeighbors(pos, r=2, metric="euclidean")
	nb4 <- findNeighbors(pos, r=1, matrix=TRUE)

	expect_equal(nb[[1L]], c(1, 2, 10, 11))
	expect_equal(nb2[[1L]], c(1, 2, 10))
	expect_equal(nb3[[1L]], c(1, 2, 3, 10, 11, 19))
	expect_is(nb4, "sparse_mat")
	expect_equal(nb4[1,1], 1)
	expect_equal(nb4[1,2], 1)
	expect_equal(nb4[1,10], 1)
	expect_equal(nb4[1,11], 1)

})

