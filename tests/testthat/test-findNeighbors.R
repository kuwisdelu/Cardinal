require(testthat)
require(Cardinal)

context("findNeighbors + friends")

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

test_that("spatialWeights", {

	co <- as.matrix(expand.grid(x=1:9, y=1:9))
	nb <- findNeighbors(co, r=1)
	wts <- spatialWeights(co, r=1)
	sd <- 3 / 4

	d1 <- matter::rowdist(co[nb[[1]],,drop=FALSE], co[1,,drop=FALSE])
	w1 <- exp(-d1^2 / (2 * sd^2))

	d2 <- matter::rowdist(co[nb[[2]],,drop=FALSE], co[2,,drop=FALSE])
	w2 <- exp(-d2^2 / (2 * sd^2))

	expect_equal(wts[[1L]], as.vector(w1))
	expect_equal(wts[[2L]], as.vector(w2))

})

test_that("spatialDists", {

	set.seed(1)
	x <- matrix(runif(81 * 6), nrow=81, ncol=6)
	y <- matrix(runif(5 * 6), nrow=5, ncol=6)
	co <- as.matrix(expand.grid(x=1:9, y=1:9))
	nb <- findNeighbors(co, r=1)
	wts <- spatialWeights(co, r=1)
	dxy <- spatialDists(x, y, co, r=1)

	ds <- matter::rowDists(x, y, BPPARAM=getCardinalBPPARAM())
	FUN <- function(nb, w) colSums(w * ds[nb,,drop=FALSE]) / sum(w)

	expect_equal(dxy, t(mapply(FUN, nb, wts)))

})
