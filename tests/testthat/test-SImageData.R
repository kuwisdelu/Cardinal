require(testthat)

context("SImageData class")

test_that("SImageData validity", {
	
	expect_true(validObject(new("SImageData")))
	expect_true(validObject(SImageData()))

	data0 <- array(1:27, dim=c(3,3,3))
	expect_true(validObject(SImageData(data=data0)))

	coord <- expand.grid(x=1:3, y=1:3)
	expect_true(validObject(SImageData(data=data0, coord=coord)))

	data1 <- matrix(1:27, nrow=3)
	sdata1 <- SImageData(data=data1)
	expect_true(validObject(sdata1))

})

test_that("SImageData accessors and assignment", {

	data0 <- array(1:27, dim=c(Features=3,x=3,y=3))
	sdata0 <- SImageData(data=data0)
	expect_equal(sdata0[], data0)
	
	expect_equivalent(sdata0[1,,], data0[1,,])
	expect_equivalent(sdata0[,1,], data0[,1,])
	expect_equivalent(sdata0[,,1], data0[,,1])

	expect_identical(dim(sdata0[1,1,,drop=TRUE]), NULL)
	expect_equivalent(dim(sdata0[1,1,,drop=FALSE]), c(1,1,3))

	data1 <- matrix(1:27, nrow=3)
	sdata1 <- SImageData(data=data1)
	expect_equal(iData(sdata1), data1)

	coord1 <- expand.grid(x=1:3, y=1:3)
	sdata1 <- SImageData(data=data1, coord=coord1)
	expect_equal(sdata1[], data0)

	new.values <- 101:103
	iData(sdata1)[,1] <- new.values
	expect_equal(iData(sdata1)[,1], new.values)

	old.values <- new.values
	new.values <- 201:203
	sdata2 <- sdata1
	iData(sdata2)[,1] <- new.values
	expect_equal(iData(sdata2)[,1], new.values)
	expect_equal(iData(sdata1)[,1], old.values)

})

test_that("SImageData compatibility", {

	data0 <- Hashmat(nrow=4, ncol=4)
	data0[] <- diag(4)
	sdata0 <- SImageData(data=data0, coord=expand.grid(x=1:2, y=1:2))
	expect_equivalent(iData(sdata0)[], diag(4))

	data1 <- array(diag(4), dim=c(Features=4,x=2,y=2))
	expect_equal(sdata0[], data1)

})

test_that("SImageData combine", {

	data1 <- matrix(1:27, nrow=3)
	sdata1 <- SImageData(data=data1)
	sdata2 <- sdata1

	coord(sdata1)$sample <- factor(1)
	coord(sdata2)$sample <- factor(2)

	sdata3 <- combine(sdata1, sdata2)

	expect_true(validObject(sdata3))

	expect_equivalent(iData(sdata3), cbind(iData(sdata1), iData(sdata2)))

})

# test_that("SImageData combine", {

# 	data1 <- array(1:27, dim=c(3,3,3))
# 	sdata1 <- SImageData(data=data1)
# 	dim(data1) <- c(3, 9)
# 	expect_equal(iData(sdata1), data1)

# 	sdata2 <- sdata1
# 	featureNames(sdata1) <- 1:3
# 	pixelNames(sdata1) <- 1:9
# 	featureNames(sdata2) <- c(1:3)
# 	pixelNames(sdata2) <- c(10, 2:9)

# 	x1 <- iData(sdata1)
# 	x2 <- iData(sdata1)
# 	dimnames(x1) <- list(featureNames(sdata1), pixelNames(sdata1))
# 	dimnames(x2) <- list(featureNames(sdata2), pixelNames(sdata2))

# 	combdata <- combine(sdata1, sdata2)
# 	expect_equivalent(iData(combdata), combine(x1, x2))

# 	featureNames(sdata2) <- 2:4
# 	expect_error(combine(sdata1, sdata2))

# 	featureNames(sdata2) <- 1:3

# 	sdata3 <- sdata2
# 	featureNames(sdata3) <- 1:3
# 	pixelNames(sdata3) <- 11:19

# 	x3 <- iData(sdata3)
# 	dimnames(x3) <- list(featureNames(sdata3), pixelNames(sdata3))

# 	multicombdata <- combine(sdata1, sdata2, sdata3)
# 	expect_equivalent(iData(multicombdata), combine(x1, x2, x3))

# })

