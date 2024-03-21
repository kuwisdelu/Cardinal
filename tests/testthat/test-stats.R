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

