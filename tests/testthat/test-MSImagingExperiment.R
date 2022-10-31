require(testthat)
require(Cardinal)

context("MSImagingExperiment class")

test_that("MSImagingExperiment validity", {
	
	expect_true(validObject(new("MSImagingExperiment")))

	data <- matrix(1:9^2, nrow=9, ncol=9)
	mz <- seq(from=100, to=105, length.out=9)
	coord <- expand.grid(x=1:3, y=1:3)

	idata <- ImageArrayList(data)
	fdata <- MassDataFrame(mz=mz)
	pdata <- PositionDataFrame(coord=coord)

	x <- MSImagingExperiment(
		imageData=idata,
		featureData=fdata,
		pixelData=pdata)

	expect_true(validObject(x))

	expect_true(is(x, "MSImagingExperiment"))

	y1 <- MSImagingExperiment(
		imageData=data,
		featureData=fdata,
		pixelData=pdata)

	expect_true(is(y1, "MSContinuousImagingExperiment"))

	y2 <- MSImagingExperiment(
		imageData=matter::matter_mat(data),
		featureData=fdata,
		pixelData=pdata)

	expect_true(is(y2, "MSContinuousImagingExperiment"))

	y3 <- MSImagingExperiment(
		imageData=matter::sparse_mat(data),
		featureData=fdata,
		pixelData=pdata)

	expect_true(is(y3, "MSProcessedImagingExperiment"))

})

test_that("MSImagingExperiment accessors", {

	data <- matrix(1:9^2, nrow=9, ncol=9)
	mz <- seq(from=100, to=105, length.out=9)
	coord <- expand.grid(x=1:3, y=1:3)

	idata <- ImageArrayList(data)
	fdata <- MassDataFrame(mz=mz)
	pdata <- PositionDataFrame(coord=coord)

	x <- MSImagingExperiment(
		imageData=idata,
		featureData=fdata,
		pixelData=pdata,
		centroided=FALSE)

	expect_equal(mz(x), mz(fdata))

	expect_equal(spectra(x), idata[[1]])

	expect_equal(centroided(x), FALSE)

})

test_that("MSImagingExperiment subsetting", {

	data <- matrix(1:9^2, nrow=9, ncol=9)
	mz <- seq(from=100, to=105, length.out=9)
	coord <- expand.grid(x=1:3, y=1:3)

	idata <- ImageArrayList(data)
	fdata <- MassDataFrame(mz=mz)
	pdata <- PositionDataFrame(coord=coord)

	x <- MSImagingExperiment(
		imageData=idata,
		featureData=fdata,
		pixelData=pdata,
		centroided=FALSE)

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

test_that("MSImagingExperiment binding", {

	data <- matrix(1:9^2, nrow=9, ncol=9)
	mz <- seq(from=100, to=105, length.out=9)
	coord <- expand.grid(x=1:3, y=1:3)

	idata <- ImageArrayList(data)
	fdata <- MassDataFrame(mz=mz)
	pdata <- PositionDataFrame(coord=coord)

	x <- MSImagingExperiment(
		imageData=idata,
		featureData=fdata,
		pixelData=pdata,
		centroided=FALSE)

	expect_error(rbind(x, x))

	expect_true(validObject(cbind(x, x)))

	x2 <- x

	mz(x2) <- mz(x) + diff(range(mz(x))) + diff(mz(x))[1]

	y <- rbind(x, x2)

	expect_equal(iData(y), rbind(iData(x), iData(x2)))
	expect_equal(pData(y), cbind(pData(x), pData(x2)))
	expect_equal(fData(y), rbind(fData(x), fData(x2)))

	x3 <- x

	coord(x3)$x <- coord(x)$x + max(coord(x)$x)
	run(x3) <- rep(factor(2), ncol(x))

	z <- cbind(x, x3)

	expect_equal(iData(z), cbind(iData(x), iData(x3)))
	expect_equal(pData(z), rbind(pData(x), pData(x3)))
	expect_equal(fData(z), cbind(fData(x), fData(x3)))

})
