require(testthat)
require(Cardinal)

context("crossValidate")

test_that("crossValidate", {

	set.seed(1)
	ms <- simulateImage(preset=2, dim=c(10L, 10L), nrun=2,
		representation="centroid")
	ms$class <- makeFactor(circle=ms$circle, square=ms$square)
	
	ncomp <- 1:3
	ans <- crossValidate(PLS, ms, ms$class, ncomp=ncomp)
	ans2 <- crossValidate(OPLS, ms, ms$class, ncomp=ncomp)

	expect_length(ans$scores, nrun(ms))
	expect_length(ans2$scores, nrun(ms))
	expect_equal(nrow(ans$average), length(ncomp))
	expect_equal(nrow(ans2$average), length(ncomp))

	s <- c(0,2,4,6,8)
	ans3 <- crossValidate(spatialShrunkenCentroids, ms, ms$class, s=s)

	expect_length(ans3$scores, nrun(ms))
	expect_equal(nrow(ans3$average), length(s))

})

