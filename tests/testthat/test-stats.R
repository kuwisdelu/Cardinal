require(testthat)
require(Cardinal)

context("stats")

test_that("PCA", {

	set.seed(1)
	s <- simulateImage(preset=1, dim=c(10L, 10L))
	pc <- PCA(s, ncomp=2)
	pred <- predict(pc, newdata=s)
	
	x <- t(spectra(s))
	pc2 <- prcomp(x)
	pred2 <- predict(pc2, newdata=x)

	expect_equal(pc$sdev, pc2$sdev[1:2])
	expect_equal(abs(pc$x), abs(pc2$x[,1:2]))
	expect_equal(abs(pc$rotation), abs(pc2$rotation[,1:2]))
	expect_equal(abs(pred), abs(pred2[,1:2]))

})

test_that("NMF", {

	set.seed(1)
	s <- simulateImage(preset=1, dim=c(10L, 10L))
	mf <- NMF(s, ncomp=2, method="als")
	pred <- predict(mf, newdata=s)
	
	mf2 <- NMF(s, ncomp=2, method="mult")
	pred2 <- predict(mf2, newdata=s)

	expect_equivalent(mf$x, pred, tolerance=0.05)
	expect_equivalent(mf2$x, pred2, tolerance=0.05)

})

test_that("PLS", {

	set.seed(1)
	s <- simulateImage(preset=1, dim=c(10L, 10L))
	s$class <- makeFactor(yes=s$circle, no=!s$circle)
	pl <- PLS(s, s$class, ncomp=2, method="nipals")
	pred <- predict(pl, type="class")
	pred01 <- predict(pl, newdata=s, ncomp=1)
	pred02 <- predict(pl, newdata=s, ncomp=2)
	pred012 <- predict(pl, newdata=s, ncomp=1:2)

	expect_is(pred, "factor")
	expect_equal(levels(pred), c("yes", "no"))
	expect_equivalent(pred01, pred012[,,1L])
	expect_equivalent(pred02, pred012[,,2L])
	expect_equivalent(fitted(pl), pred02)

	pl2 <- PLS(s, s$class, ncomp=2, method="simpls")
	pred2 <- predict(pl2, newdata=s)
	pl3 <- PLS(s, s$class, ncomp=2, method="kernel1")
	pred3 <- predict(pl3, newdata=s)
	pl4 <- PLS(s, s$class, ncomp=2, method="kernel2")
	pred4 <- predict(pl4, newdata=s)

	expect_equivalent(fitted(pl2), pred2)
	expect_equivalent(fitted(pl3), pred3)
	expect_equivalent(fitted(pl4), pred4)

})

test_that("OPLS", {

	set.seed(1)
	s <- simulateImage(preset=1, dim=c(10L, 10L))
	s$class <- makeFactor(yes=s$circle, no=!s$circle)
	op <- OPLS(s, s$class, ncomp=1:2)

	pred <- predict(op, type="class")
	pred1 <- predict(op, newdata=s, ncomp=1)
	pred2 <- predict(op, newdata=s, ncomp=2)

	expect_equal(pred, s$class)
	expect_equal(pred1, op$regressions[[1L]]$fitted.values)
	expect_equal(pred2, op$regressions[[2L]]$fitted.values)

})

