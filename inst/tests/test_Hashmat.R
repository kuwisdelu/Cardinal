require(testthat)

context("Hashmat")

test_that("Hashmat accessing", {
	
	expect_true(validObject(new("Hashmat")))
	expect_true(validObject(Hashmat()))

	sdat <- list(c(a=1, b=2, c=3),
		c(a=4, c=5),
		c(b=6, c=7, d=8),
		numeric(),
		c(a=9))
	x <- Hashmat(data=dat, keys=c("a", "b", "c", "d", "e"))
	expect_true(validObject(x))

	ddat <- c(c(1, 2, 3, 0, 0),
			c(4, 0, 5, 0, 0),
			c(0, 6, 7, 8, 0),
			c(0, 0, 0, 0, 0),
			c(9, 0, 0, 0, 0))
	y <- matrix(ddat, nrow=5, ncol=5)
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
