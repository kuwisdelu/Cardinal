require(testthat)
require(Cardinal)

context("SparseImagingExperiment class")

test_that("SparseImagingExperiment validity", {
	
	expect_true(validObject(new("SparseImagingExperiment")))

	data <- matrix(1:9^2, nrow=9, ncol=9)
	t <- seq_len(9)
	a <- seq_len(9)
	coord <- expand.grid(x=1:3, y=1:3)

	idata <- ImageArrayList(data)
	fdata <- DataFrame(t=t)
	pdata <- PositionDataFrame(coord=coord, a=a)

	x <- SparseImagingExperiment(
		imageData=idata,
		featureData=fdata,
		pixelData=pdata)

	expect_true(validObject(x))

})

test_that("SparseImagingExperiment accessors", {

	data <- matrix(1:9^2, nrow=9, ncol=9)
	t <- seq_len(9)
	a <- seq_len(9)
	coord <- expand.grid(x=1:3, y=1:3)

	idata <- ImageArrayList(data)
	fdata <- DataFrame(t=t)
	pdata <- PositionDataFrame(coord=coord, a=a)

	x <- SparseImagingExperiment(
		imageData=idata,
		featureData=fdata,
		pixelData=pdata)

	expect_equal(imageData(x), idata)
	expect_equal(iData(x), idata[[1]])
	
	expect_equal(pixelData(x), pdata)
	expect_equal(pData(x), pdata)
	
	expect_equal(featureData(x), fdata)
	expect_equal(fData(x), fdata)

	expect_equal(processingData(x), SimpleList())

	expect_equal(dim(x), c(Features=nrow(fdata), Pixels=nrow(pdata)))

	expect_equal(x[["a"]], pdata[["a"]])

	expect_equal(x$a, pdata$a)

	expect_equal(coord(x), coord(pdata))

	expect_equal(dims(x), dims(pdata))

})

test_that("SparseImagingExperiment subsetting", {

	data <- matrix(1:9^2, nrow=9, ncol=9)
	t <- seq_len(9)
	a <- seq_len(9)
	coord <- expand.grid(x=1:3, y=1:3)

	idata <- ImageArrayList(data)
	fdata <- DataFrame(t=t)
	pdata <- PositionDataFrame(coord=coord, a=a)

	x <- SparseImagingExperiment(
		imageData=idata,
		featureData=fdata,
		pixelData=pdata)

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

test_that("SparseImagingExperiment binding", {

	data <- matrix(1:9^2, nrow=9, ncol=9)
	t <- seq_len(9)
	a <- seq_len(9)
	coord <- expand.grid(x=1:3, y=1:3)

	idata <- ImageArrayList(data)
	fdata <- DataFrame(t=t)
	pdata <- PositionDataFrame(coord=coord, a=a)

	x <- SparseImagingExperiment(
		imageData=idata,
		featureData=fdata,
		pixelData=pdata)

	y <- rbind(x, x)

	expect_equal(iData(y), rbind(iData(x), iData(x)))
	expect_equal(pData(y), cbind(pData(x), pData(x)))
	expect_equal(fData(y), rbind(fData(x), fData(x)))

	expect_error(cbind(x, x))

	x2 <- x

	coord(x2)$x <- coord(x)$x + max(coord(x)$x)
	run(x2) <- rep(factor(2), ncol(x))

	expect_error(rbind(x, x2))

	z <- cbind(x, x2)

	expect_equal(iData(z), cbind(iData(x), iData(x2)))
	expect_equal(pData(z), rbind(pData(x), pData(x2)))
	expect_equal(fData(z), cbind(fData(x), fData(x2)))

})
