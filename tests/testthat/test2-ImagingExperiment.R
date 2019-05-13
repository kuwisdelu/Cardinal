require(testthat)
require(Cardinal)

context("ImagingExperiment class")

test_that("ImagingExperiment validity", {
	
	expect_error(new("ImagingExperiment"))

	setClass("XImagingExperiment", contains="ImagingExperiment")

	expect_true(validObject(new("XImagingExperiment")))

})

test_that("ImagingExperiment accessors", {

	setClass("XImagingExperiment", contains="ImagingExperiment")

	data <- matrix(1:9^2, nrow=9)
	t <- seq_len(9)
	a <- seq_len(9)

	idata <- ImageList(data)
	fdata <- DataFrame(t=t)
	pdata <- DataFrame(a=a)

	x <- new("XImagingExperiment",
		imageData=idata,
		featureData=fdata,
		elementMetadata=pdata)

	expect_true(validObject(x))

	expect_equal(imageData(x), idata)
	expect_equal(iData(x), idata[[1]])
	
	expect_equal(phenoData(x), pdata)
	expect_equal(pData(x), pdata)
	expect_null(pixelData(x))
	
	expect_equal(featureData(x), fdata)
	expect_equal(fData(x), fdata)

	expect_equal(dim(x), c(nrow(fdata), nrow(pdata)))

	expect_equal(x[["a"]], pdata[["a"]])

	expect_equal(x$a, pdata$a)

})

test_that("ImagingExperiment subsetting", {

	setClass("XImagingExperiment", contains="ImagingExperiment")

	data <- matrix(1:9^2, nrow=9)
	t <- seq_len(9)
	a <- seq_len(9)

	idata <- ImageArrayList(data)
	fdata <- DataFrame(t=t)
	pdata <- DataFrame(a=a)

	x <- new("XImagingExperiment",
		imageData=idata,
		featureData=fdata,
		elementMetadata=pdata)

	xi <- x[1:3,]

	expect_equal(iData(xi), data[1:3,])
	expect_equal(pData(xi), pdata)
	expect_equal(fData(xi), fdata[1:3,,drop=FALSE])

	xj <- x[,1:3]

	expect_equal(iData(xj), data[,1:3])
	expect_equal(pData(xj), pdata[1:3,,drop=FALSE])
	expect_equal(fData(xj), fdata)

	xij <- x[1:3,1:3]

	expect_equal(iData(xij), data[1:3,1:3])
	expect_equal(pData(xij), pdata[1:3,,drop=FALSE])
	expect_equal(fData(xij), fdata[1:3,,drop=FALSE])

})

test_that("ImagingExperiment binding", {

	setClass("XImagingExperiment", contains="ImagingExperiment")

	data <- matrix(1:9^2, nrow=9)
	t <- seq_len(9)
	a <- seq_len(9)

	idata <- ImageArrayList(data)
	fdata <- DataFrame(t=t)
	pdata <- DataFrame(a=a)

	x <- new("XImagingExperiment",
		imageData=idata,
		featureData=fdata,
		elementMetadata=pdata)

	y <- rbind(x, x)

	expect_equal(iData(y), rbind(iData(x), iData(x)))
	expect_equal(pData(y), cbind(pData(x), pData(x)))
	expect_equal(fData(y), rbind(fData(x), fData(x)))

	z <- cbind(x, x)

	expect_equal(iData(z), cbind(iData(x), iData(x)))
	expect_equal(pData(z), rbind(pData(x), pData(x)))
	expect_equal(fData(z), cbind(fData(x), fData(x)))

})



