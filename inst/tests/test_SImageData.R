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
	sdata1 <- SImageData(data=data1, storageMode="immutableEnvironment")
	expect_equal(iData(sdata1), data1)

	coord1 <- expand.grid(x=1:3, y=1:3)
	sdata1 <- SImageData(data=data1, coord=coord1, storageMode="immutableEnvironment")
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

	storageMode(sdata1) <- "lockedEnvironment"
	expect_error(iData(sdata1)[,1] <- new.values)

	storageMode(sdata1) <- "immutableEnvironment"
	dimnames(iData(sdata1)) <- list(1:3, 1:9)
	dimnames(iData(sdata2)) <- list(1:3, c(10, 2:9))
	combdata <- combine(sdata1, sdata2)
	expect_equivalent(iData(combdata), combine(iData(sdata1), iData(sdata2)))

	sdata3 <- sdata2
	dimnames(iData(sdata3)) <- list(1:3, 11:19)	
	multicombdata <- combine(sdata1, sdata2, sdata3)
	expect_equivalent(iData(multicombdata), combine(iData(sdata1), iData(sdata2), iData(sdata3)))

})

test_that("SImageData sparse signal", {

	data0 <- Hashmat(nrow=4, ncol=4)
	data0[] <- diag(4)
	sdata0 <- SImageData(data=data0, coord=expand.grid(x=1:2, y=1:2))
	expect_equivalent(iData(sdata0)[], diag(4))

	data1 <- array(diag(4), dim=c(Features=4,x=2,y=2))
	expect_equal(sdata0[], data1)

})

# test_that("SImageData copying", {

# 	coord <- expand.grid(x=1:100, y=1:100)
# 	data1 <- matrix(0, nrow=9000, ncol=100*100)
# 	print(gc())

# 	sdata1 <- SImageData(data=data1, coord=coord)
# 	print(gc())

# 	pixelNames(sdata1) <- paste("p", 1:(100*100))
# 	print(gc())

# 	pixelNames(sdata1) <- paste("p", (100*100):1)
# 	print(gc())

# 	featureNames(sdata1) <- paste("f", 1:9000)
# 	print(gc())

# 	featureNames(sdata1) <- paste("f", 9000:1)
# 	print(gc())

# })

