require(testthat)
require(Cardinal)

context("spectrapply")

test_that("spectrapply - SpectralImagingArrays", {

	set.seed(1, kind="default")
	x <- replicate(9, rlnorm(100), simplify=FALSE)
	t1 <- replicate(9, sort(runif(100)), simplify=FALSE)
	t2 <- replicate(9, sort(runif(100)), simplify=FALSE)

	sa <- SpectralImagingArrays(
		spectraData=list(x=x, t1=t1, t2=t2),
		pixelData=PositionDataFrame(expand.grid(x=1:3, y=1:3)))

	xout <- spectrapply(sa,
		FUN=function(x, t1, t2, ...) x,
		spectra="x", index=c("t1", "t2"),
		simplify=FALSE)
	
	t1out <- spectrapply(sa,
		FUN=function(x, t1, t2, ...) t1,
		spectra="x", index=c("t1", "t2"),
		simplify=FALSE)

	t2out <- spectrapply(sa,
		FUN=function(x, t1, t2, ...) t2,
		spectra="x", index=c("t1", "t2"),
		simplify=FALSE)

	expect_equal(spectra(sa, "x"), xout)
	expect_equal(spectra(sa, "t1"), t1out)
	expect_equal(spectra(sa, "t2"), t2out)

})

test_that("spectrapply - SpectralImagingExperiment", {

	set.seed(1, kind="default")
	x <- replicate(9, rlnorm(100), simplify=TRUE)
	t1 <- sort(runif(100))
	t2 <- sort(runif(100))

	se <- SpectralImagingExperiment(
		spectraData=list(x=x),
		featureData=DataFrame(t1=t1, t2=t2),
		pixelData=PositionDataFrame(expand.grid(x=1:3, y=1:3)))

	xout <- spectrapply(se,
		FUN=function(x, t1, t2, ...) x,
		spectra="x", index=c("t1", "t2"),
		simplify=TRUE)
	
	t1out <- spectrapply(se,
		FUN=function(x, t1, t2, ...) t1,
		spectra="x", index=c("t1", "t2"),
		simplify=TRUE)

	t2out <- spectrapply(se,
		FUN=function(x, t1, t2, ...) t2,
		spectra="x", index=c("t1", "t2"),
		simplify=TRUE)

	expect_equal(spectra(se, "x"), xout)
	expect_equal(featureData(se)$t1, t1out[,1L])
	expect_equal(featureData(se)$t2, t2out[,2L])

})

test_that("spectrapply - MSImagingArrays", {

	path <- CardinalIO::exampleImzMLFile("processed")
	msa <- readImzML(path, memory=TRUE)

	xout <- spectrapply(msa, function(x, t, ...) x, simplify=FALSE)
	mzout <- spectrapply(msa, function(x, t, ...) t, simplify=FALSE)

	expect_equal(intensity(msa), xout)
	expect_equal(mz(msa), mzout)

})

test_that("spectrapply - MSImagingExperiment", {

	path <- CardinalIO::exampleImzMLFile("continuous")
	mse <- readImzML(path, memory=TRUE)

	xout <- spectrapply(mse, function(x, t, ...) x)
	mzout <- spectrapply(mse, function(x, t, ...) t)

	expect_equal(spectra(mse), xout)
	expect_equal(mz(mse), mzout[,1L])

})
