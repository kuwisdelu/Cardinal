require(testthat)

context("SImageSet class")

test_that("SImageSet validity", {
	
	expect_true(validObject(new("SImageSet")))
	expect_true(validObject(SImageSet()))

	data0 <- array(1:27, dim=c(3,3,3))
	sset0 <- SImageSet(data=data0)
	expect_true(validObject(sset0))

	data1 <- matrix(1:27, nrow=3)
	coord <- expand.grid(x=1:3, y=1:3)
	sset1 <- SImageSet(data=data1, coord=coord)
	expect_true(validObject(sset1))

})

test_that("SImageSet imageData", {

	data <- matrix(1:27, nrow=3)
	coord <- expand.grid(x=1:3, y=1:3)
	sset <- SImageSet(data=data, coord=coord)

	expect_equivalent(iData(sset), data)
	
	dim(data) <- c(Features=3, x=3, y=3)
	expect_identical(imageData(sset)[], data)

	expect_identical(iData(sset), iData(sset))

	sset2 <- sset
	iData(sset2) <- matrix(27:1, nrow=3)
	expect_equal(sum(iData(sset2) == iData(sset)), 1)

	sset3 <- SImageSet(data=matrix(1))

})

test_that("SImageSet pixelData", {

	data <- matrix(1:27, nrow=3)
	coord <- expand.grid(x=1:3, y=1:3)
	sset <- SImageSet(data=data, coord=coord)

	expect_identical(pixelNames(imageData(sset)), pixelNames(sset))

	sset[["test"]] <- rnorm(9)
	expect_identical(pData(sset)$test, sset$test)

	expect_equivalent(coord(sset), coord)

	sset2 <- sset
	coord(sset2) <- coord(sset2)[9:1,]
	expect_equal(sum(imageData(sset2)[1,,] == imageData(sset)[1,,]), 1)

	coordLabels(sset2) <- c("x1", "x2")
	expect_identical(rownames(dims(sset2))[-1], c("x1", "x2"))

	pixelNames(sset) <- paste("p", 1:9)
	expect_identical(pixelNames(imageData(sset)), paste("p", 1:9))
	expect_identical(pixelNames(pixelData(sset)), paste("p", 1:9))
	expect_identical(pixelNames(sset), paste("p", 1:9))

})

test_that("SImageSet featureData", {

	data <- matrix(1:27, nrow=3)
	coord <- expand.grid(x=1:3, y=1:3)
	sset <- SImageSet(data=data, coord=coord)

	expect_identical(featureNames(imageData(sset)), featureNames(sset))

	expect_error(fvarLabels(sset) <- "test")

	test <- rnorm(3)
	fData(sset)$test <- test
	expect_identical(fData(sset)[["test"]], test)

	featureNames(sset) <- paste("f", 1:3)
	expect_identical(featureNames(imageData(sset)), paste("f", 1:3))
	expect_identical(featureNames(featureData(sset)), paste("f", 1:3))
	expect_identical(featureNames(sset), paste("f", 1:3))

})

test_that("SImageSet protocolData", {

	data <- matrix(1:27, nrow=3)
	coord <- expand.grid(x=1:3, y=1:3)
	sset <- SImageSet(data=data, coord=coord)
	
	expect_identical(sampleNames(protocolData(sset)), sampleNames(pixelData(sset)))

	sampleNames(sset) <- "sample 1"
	expect_identical(sampleNames(pixelData(sset)), "sample 1")
	expect_identical(sampleNames(protocolData(sset)), "sample 1")

})

test_that("SImageSet subsetting", {

	data <- matrix(1:27, nrow=3)
	coord <- expand.grid(x=1:3, y=1:3)
	sset <- SImageSet(data=data, coord=coord)

	expect_true(validObject(sset[1:2,1:6]))
	expect_true(validObject(sset[1,]))
	expect_true(validObject(sset[,1]))

})

test_that("SImageSet pixelApply", {

	data <- matrix(1:256, nrow=4)
	coord <- expand.grid(x=1:4, y=1:4, z=1:4)
	sset <- SImageSet(data=data, coord=coord)

	fData(sset)$fflag <- rep(c(TRUE, FALSE), 2)
	pData(sset)$pflag <- rep(c(TRUE, FALSE), 32)

	tmp <- pixelApply(sset, max, .feature.groups=fflag)
	expect_equivalent(pixelApply(sset, max, .feature=fflag), tmp["TRUE",])
	expect_equivalent(pixelApply(sset, function(x) max(x[fflag])), tmp["TRUE",])

	tmp2 <- pixelApply(sset, function(x) .Index)
	expect_equivalent(tmp2, pixels(sset))

	# tmp3 <- pixelApply(sset, function(x) coord(.Object)[.Index,])
	# expect_equivalent(t(tmp3), as.matrix(coord(sset)))

})

test_that("SImageSet featureApply", {

	data <- matrix(1:256, nrow=4)
	coord <- expand.grid(x=1:4, y=1:4, z=1:4)
	sset <- SImageSet(data=data, coord=coord)

	fData(sset)$fflag <- rep(c(TRUE, FALSE), 2)
	pData(sset)$pflag <- rep(c(TRUE, FALSE), 32)

	tmp <- featureApply(sset, max, .pixel.groups=pflag)
	expect_equivalent(featureApply(sset, max, .pixel=pflag), tmp["TRUE",])
	expect_equivalent(featureApply(sset, function(x) max(x[pflag])), tmp["TRUE",])

})

