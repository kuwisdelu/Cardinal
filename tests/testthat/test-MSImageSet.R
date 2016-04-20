require(testthat)

context("MSImageSet class")

test_that("MSImageSet validity", {
	
	expect_true(validObject(new("MSImageSet")))
	expect_true(validObject(MSImageSet()))

	spectra0 <- array(1:27, dim=c(3,3,3))
	msset0 <- MSImageSet(spectra=spectra0)
	expect_true(validObject(msset0))

	spectra1 <- matrix(1:27, nrow=3)
	coord <- expand.grid(x=1:3, y=1:3)
	msset1 <- MSImageSet(spectra=spectra1, coord=coord)
	expect_true(validObject(msset1))

	mz <- c(100, 200, 300)
	msset2 <- MSImageSet(spectra=spectra1, mz=mz, coord=coord)
	expect_true(validObject(msset2))

})

test_that("MSImageSet imageData", {

	mz <- c(101, 102, 103)
	spectra <- matrix(1:27, nrow=3)
	coord <- expand.grid(x=1:3, y=1:3)
	msset <- MSImageSet(spectra=spectra, mz=mz, coord=coord)

	expect_equivalent(spectra(msset), spectra)
	
	dim(spectra) <- c(Features=3, x=3, y=3)
	expect_identical(imageData(msset)[], spectra)

	expect_identical(iData(msset), spectra(msset))

	msset2 <- msset
	spectra(msset2) <- matrix(27:1, nrow=3)
	expect_equal(sum(spectra(msset2) == spectra(msset)), 1)

})

test_that("MSImageSet pixelData", {

	mz <- c(101, 102, 103)
	spectra <- matrix(1:27, nrow=3)
	coord <- expand.grid(x=1:3, y=1:3)
	msset <- MSImageSet(spectra=spectra, mz=mz, coord=coord)

	expect_identical(pixelNames(imageData(msset)), pixelNames(msset))

	expect_equivalent(pixels(msset, x=1:2, y=1:2), c(1,2,4,5))

	pData(msset)$flag <- rep(c(TRUE, FALSE,FALSE), 3)
	expect_equivalent(pixels(msset, x=1:2, y=1:2, flag=TRUE), c(1,4))
	expect_equivalent(pixels(msset, flag=TRUE), c(1,4,7))

	msset[["test"]] <- rnorm(9)
	expect_identical(pData(msset)$test, msset$test)

	expect_equivalent(coord(msset), coord)

	msset2 <- msset
	coord(msset2) <- coord(msset2)[9:1,]
	expect_equal(sum(imageData(msset2)[1,,] == imageData(msset)[1,,]), 1)

	coordLabels(msset2) <- c("x1", "x2")
	expect_identical(rownames(dims(msset2))[-1], c("x1", "x2"))

	pixelNames(msset) <- paste("p", 1:9)
	expect_identical(pixelNames(imageData(msset)), paste("p", 1:9))
	expect_identical(pixelNames(pixelData(msset)), paste("p", 1:9))
	expect_identical(pixelNames(msset), paste("p", 1:9))

})

test_that("MSImageSet featureData", {

	mz <- c(101, 102, 103)
	spectra <- matrix(1:27, nrow=3)
	coord <- expand.grid(x=1:3, y=1:3)
	msset <- MSImageSet(spectra=spectra, mz=mz, coord=coord)

	expect_identical(featureNames(imageData(msset)), featureNames(msset))

	expect_identical(mz(msset), mz)

	expect_equivalent(features(msset, mz=102), 2)

	features(msset, mz=c(101,103))

	fData(msset)$flag <- c(TRUE, TRUE, FALSE)
	expect_equivalent(features(msset, mz=c(101, 103), flag=TRUE), 1)
	expect_equivalent(features(msset, flag=TRUE), c(1, 2))

	mz2 <- c(1001, 1002, 1003)
	mz(msset) <- mz2
	expect_identical(mz(msset), mz2)

	test <- rnorm(3)
	fData(msset)$test <- test
	expect_identical(fData(msset)[["test"]], test)

	featureNames(msset) <- paste("f", 1:3)
	expect_identical(featureNames(imageData(msset)), paste("f", 1:3))
	expect_identical(featureNames(featureData(msset)), paste("f", 1:3))
	expect_identical(featureNames(msset), paste("f", 1:3))

})

test_that("MSImageSet protocolData", {

	mz <- c(101, 102, 103)
	spectra <- matrix(1:27, nrow=3)
	coord <- expand.grid(x=1:3, y=1:3)
	msset <- MSImageSet(spectra=spectra, mz=mz, coord=coord)
	
	expect_identical(sampleNames(protocolData(msset)), sampleNames(pixelData(msset)))

	sampleNames(msset) <- "sample 1"
	expect_identical(sampleNames(pixelData(msset)), "sample 1")
	expect_identical(sampleNames(protocolData(msset)), "sample 1")

})

test_that("MSImageSet subsetting", {

	mz <- c(101, 102, 103)
	spectra <- matrix(1:27, nrow=3)
	coord <- expand.grid(x=1:3, y=1:3)
	msset <- MSImageSet(spectra=spectra, mz=mz, coord=coord)

	expect_true(validObject(msset[1:2,1:6]))
	expect_true(validObject(msset[1,]))
	expect_true(validObject(msset[,1]))

})

# test_that("MSImageSet combine", {

# 	mz <- c(101, 102, 103)
# 	coord <- expand.grid(x=1:3, y=1:3)
# 	spectra <- matrix(1:27, nrow=3)
	
# 	msset1 <- MSImageSet(spectra=spectra, mz=mz, coord=coord)
# 	msset2 <- MSImageSet(spectra=spectra, mz=mz, coord=coord)

# 	sampleNames(msset1) <- "s1"
# 	sampleNames(msset2) <- "s2"

# 	msset3 <- combine(msset1, msset2)
# 	expect_true(validObject(msset3))

# })

