require(testthat)

context("Hashmat")

test_that("Hashmat validity", {

	expect_true(validObject(new("Hashmat")))
	expect_true(validObject(Hashmat()))

	dmat <- diag(3)
	smat <- Hashmat(dmat)
	expect_equivalent(dmat, smat[])

})

test_that("Hashmat accessors", {

	smat <- list(c(a=1, b=2, c=3),
		c(a=4, c=5),
		c(b=6, c=7, d=8),
		numeric(),
		c(a=9))
	x <- new("Hashmat", data=smat, keys=c("a", "b", "c", "d", "e"), dim=c(5,5))
	expect_true(validObject(x))

	dmat <- c(c(1, 2, 3, 0, 0),
			c(4, 0, 5, 0, 0),
			c(0, 6, 7, 8, 0),
			c(0, 0, 0, 0, 0),
			c(9, 0, 0, 0, 0))
	y <- matrix(dmat, nrow=5, ncol=5)
	expect_equivalent(x[], y)

	expect_equivalent(x[2,], y[2,])
	expect_equivalent(x[2,,drop=FALSE], y[2,,drop=FALSE])
	expect_equivalent(x[,2,drop=FALSE], y[,2,drop=FALSE])
	expect_equivalent(x[2:4,2:4], y[2:4,2:4])
	expect_equivalent(x[3,3], y[3,3])
	
	expect_equal(nrow(x), 5)
	expect_equal(ncol(x), 5)

	rownames(x) <- paste("row", 1:5)
	expect_identical(rownames(x[2:4,]), paste("row", 2:4))

	colnames(x) <- paste("col", 1:5)
	expect_identical(colnames(x[,2:4]), paste("col", 2:4))

	expect_identical(dimnames(x), list(paste("row", 1:5), paste("col", 1:5)))

	dimnames(x) <- list(paste("r", 1:5), paste("c", 1:5))
	expect_identical(rownames(x[]), paste("r", 1:5))
	expect_identical(colnames(x[]), paste("c", 1:5))

})

test_that("Hashmat assignment", {

	x <- Hashmat(nrow=5, ncol=5)
	y <- matrix(0, nrow=5, ncol=5)
	expect_equivalent(x[], y)
	
	x[1,1] <- 1
	y[1,1] <- 1
	expect_equivalent(x[], y)
	
	x[2,] <- 1
	y[2,] <- 1
	expect_equivalent(x[], y)
	
	x[,2] <- 1
	y[,2] <- 1
	expect_equivalent(x[], y)
	
	x[2:4,2] <- c(0,1,0)
	y[2:4,2] <- c(0,1,0)
	expect_equivalent(x[], y)
	
	x[2,3:5] <- c(0,1,0)
	y[2,3:5] <- c(0,1,0)
	expect_equivalent(x[], y)
	
	x[3:5,3:5] <- diag(3)
	y[3:5,3:5] <- diag(3)
	expect_equivalent(x[], y)

	expect_true(all(sapply(x@data, function(xi) all(xi != 0))))

})

test_that("Hashmat combine", {

	x <- new("Hashmat", data=list(c(k1=1), c(k2=2), c(k3=3)), keys=c("k1", "k2", "k3"),
		dim=c(3,3), dimnames=list(c("r1", "r2", "r3"), c("c1", "c2", "c3")))
	y <- new("Hashmat", data=list(c(k2=2), c(k3=3), c(k4=4)), keys=c("k1", "k2", "k3", "k4"),
		dim=c(4,3), dimnames=list(c("r1", "r2", "r3", "r4"), c("c2", "c3", "c4")))
	expect_true(validObject(combine(x, y)))

	z <- matrix(0, nrow=4, ncol=4)
	diag(z) <- 1:4
	expect_equivalent(combine(x, y)[], z)

	expect_error(rbind(x, y))

	expect_error(cbind(x, y))

	expect_equivalent(cbind(x, y[1:3,,drop=NA])[], cbind(x[], y[1:3,]))

})


