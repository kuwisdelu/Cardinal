require(testthat)
require(Cardinal)

context("spatialWeights + spatialDists")

test_that("spatialWeights", {

	set.seed(1)
	x <- matrix(runif(81 * 6), nrow=81, ncol=6)
	co <- as.matrix(expand.grid(x=1:9, y=1:9))
	nb <- findNeighbors(co, r=1)
	wts <- spatialWeights(co, r=1, weights="gaussian")
	sd <- 3 / 4

	d1 <- matter::rowdist(co[nb[[1]],,drop=FALSE], co[1,,drop=FALSE])
	w1 <- exp(-d1^2 / (2 * sd^2))

	d2 <- matter::rowdist(co[nb[[2]],,drop=FALSE], co[2,,drop=FALSE])
	w2 <- exp(-d2^2 / (2 * sd^2))

	expect_equal(wts[[1L]], as.vector(w1))
	expect_equal(wts[[2L]], as.vector(w2))

	awts <- spatialWeights(x, co, r=1, weights="adaptive")
	swts <- spatialWeights(x, co, r=1, weights="adaptive",
		matrix=TRUE)

	expect_equal(lengths(wts), lengths(awts))
	expect_equal(nrow(swts), nrow(x))
	expect_equal(ncol(swts), nrow(x))

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

