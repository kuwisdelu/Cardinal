require(testthat)
require(Cardinal)

context("crossValidate")

test_that("crossValidate", {

	set.seed(1, kind="L'Ecuyer-CMRG")
	ms <- simulateImage(preset=4, dim=c(10L, 10L), nrun=2,
		centroided=TRUE)
	ms$class <- makeFactor(A=ms$circleA, B=ms$circleB)
	ms$folds <- makeFactor(
		fold1=run(ms) %in% c("runA1", "runB1"),
		fold2=run(ms) %in% c("runA2", "runB2"))
	
	ncomp <- 1:3
	ans <- crossValidate(PLS, ms, ms$class,
		ncomp=ncomp, folds=ms$folds)
	ans2 <- crossValidate(OPLS, ms, ms$class,
		ncomp=ncomp, folds=ms$folds)
	ans_mi <- crossValidate(PLS, ms, ms$class,
		ncomp=ncomp, folds=ms$folds, bags=run(ms))
	ans2_mi <- crossValidate(OPLS, ms, ms$class,
		ncomp=ncomp, folds=ms$folds, bags=run(ms))

	expect_length(ans$scores, nlevels(ms$folds))
	expect_length(ans2$scores, nlevels(ms$folds))
	expect_length(ans_mi$scores, nlevels(ms$folds))
	expect_length(ans2_mi$scores, nlevels(ms$folds))
	expect_equal(nrow(ans$average), length(ncomp))
	expect_equal(nrow(ans2$average), length(ncomp))
	expect_equal(nrow(ans_mi$average), length(ncomp))
	expect_equal(nrow(ans2_mi$average), length(ncomp))

	s <- seq(0, 1, length.out=6)
	ans3 <- crossValidate(spatialShrunkenCentroids, ms, ms$class,
		s=s, folds=ms$folds, keep.models=TRUE)
	ans3_mi <- crossValidate(spatialShrunkenCentroids, ms, ms$class,
		s=s, folds=ms$folds, bags=run(ms), keep.models=TRUE)

	expect_length(ans3$scores, nlevels(ms$folds))
	expect_equal(nrow(ans3$average), length(s))

})

