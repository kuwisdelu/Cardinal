require(testthat)

context("nipals")

test_that("NIPALS - PCA", {

	set.seed(1)
	x <- matrix(runif(100), nrow=10, ncol=10)
	
	svd <- prcomp(scale(x, scale=FALSE))
	pca <- nipals.PCA(scale(x, scale=FALSE), ncomp=10)

	expect_equal(abs(svd$rotation), abs(pca$loadings), tolerance=0.2)

	expect_equal(abs(svd$x), abs(pca$scores), tolerance=0.2)

})

test_that("NIPALS - PLS", {

	# Arithmetic example from Trygg and Wold

	x0 <- rbind(c(-1, -1),
				c(1, -1),
				c(-1, 1),
				c(1, 1))

	y <- as.matrix(c(2, 2, 0, -4))
	
	pls0 <- nipals.PLS(x0, y, ncomp=1)

	w0.1 <- c(-0.45, -0.89)
	p0.1 <- c(-0.45, -0.89)
	b0 <- c(-1, -2)

	expect_equal(w0.1, pls0$weights[,1], tolerance=0.01)
	expect_equal(p0.1, pls0$loadings[,1], tolerance=0.01)
	expect_equal(b0, pls0$coefficients[,1], tolerance=0.01)

	x1 <- rbind(c(-2.18, -2.18),
				c(1.84, -0.16),
				c(-0.48, 1.52),
				c(0.83, 0.83))

	pls1 <- nipals.PLS(x1, y, ncomp=2)

	w1.1 <- c(-0.45, -0.89)
	p1.1 <- c(-0.69, -0.77)
	b1 <- c(0.08, -1.08)

	expect_equal(w1.1, pls1$weights[,1], tolerance=0.01)
	expect_equal(p1.1, pls1$loadings[,1], tolerance=0.01)
	expect_equal(b1, pls1$coefficients[,1], tolerance=0.01)

})

test_that("NIPALS - OPLS", {

	# Arithmetic example from Trygg and Wold

	x1 <- rbind(c(-2.18, -2.18),
				c(1.84, -0.16),
				c(-0.48, 1.52),
				c(0.83, 0.83))

	y <- as.matrix(c(2, 2, 0, -4))

	opls <- nipals.OPLS(x1, y, ncomp=1)

	wo.1 <- c(-0.89, 0.45)
	po.1 <- c(-1.16, -0.09)
	to.1 <- c(0.97, -1.71, 1.11, -0.37)

	expect_equal(wo.1, opls$Oweights[,1], tolerance=0.01)
	expect_equal(po.1, opls$Oloadings[,1], tolerance=0.01)
	expect_equal(to.1, opls$Oscores[,1], tolerance=0.01)

	pls <- nipals.PLS(opls$Xnew, y, ncomp=1)

	w.1 <- c(-0.45, -0.89)
	p.1 <- c(-0.45, -0.89)
	b <- c(-0.41, -0.82)

	expect_equal(w.1, pls$weights[,1], tolerance=0.01)
	expect_equal(p.1, pls$loadings[,1], tolerance=0.01)
	expect_equal(b, pls$coefficients[,1], tolerance=0.01)

})

