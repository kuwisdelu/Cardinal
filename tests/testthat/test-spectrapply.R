require(testthat)
require(Cardinal)

context("spectrapply")

test_that("spectrapply - SpectralImagingArrays", {

	set.seed(1, kind="default")
	x <- replicate(9, rlnorm(100), simplify=FALSE)
	t1 <- replicate(9, sort(runif(100)), simplify=FALSE)
	t2 <- replicate(9, sort(runif(100)), simplify=FALSE)
	t3 <- replicate(9, sort(runif(100)), simplify=FALSE)

	sa <- SpectralImagingArrays(
		spectraData=list(x=x, t1=t1, t2=t2, t3=t3),
		pixelData=PositionDataFrame(expand.grid(x=1:3, y=1:3)))

	xout <- spectrapply(sa,
		FUN=function(x, t1, ...) x,
		spectra="x", index=c("t1", "t2"),
		simplify=FALSE)
	
	t1out <- spectrapply(sa,
		FUN=function(x, t1, ...) t1,
		spectra="x", index="t1",
		simplify=FALSE)

	expect_equal(spectra(sa, "x"), xout)
	expect_equal(spectra(sa, "t1"), t1out)

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

	xout <- spectrapply(sa,
		FUN=function(x, t1, t2, t3, ...) x,
		spectra="x", index=c("t1", "t2", "t3"),
		simplify=FALSE)
	
	t1out <- spectrapply(sa,
		FUN=function(x, t1, t2, t3, ...) t1,
		spectra="x", index=c("t1", "t2", "t3"),
		simplify=FALSE)

	t2out <- spectrapply(sa,
		FUN=function(x, t1, t2, t3, ...) t2,
		spectra="x", index=c("t1", "t2", "t3"),
		simplify=FALSE)

	t3out <- spectrapply(sa,
		FUN=function(x, t1, t2, t3, ...) t3,
		spectra="x", index=c("t1", "t2", "t3"),
		simplify=FALSE)

	expect_equal(spectra(sa, "x"), xout)
	expect_equal(spectra(sa, "t1"), t1out)
	expect_equal(spectra(sa, "t2"), t2out)
	expect_equal(spectra(sa, "t3"), t3out)

})

test_that("spectrapply - SpectralImagingExperiment", {

	set.seed(1, kind="default")
	x <- replicate(9, rlnorm(100), simplify=TRUE)
	t <- sort(runif(100))

	se <- SpectralImagingExperiment(
		spectraData=list(x=x),
		featureData=DataFrame(t=t),
		pixelData=PositionDataFrame(expand.grid(x=1:3, y=1:3)))

	xout <- spectrapply(se,
		FUN=function(x, t, ...) x,
		spectra="x", index="t",
		simplify=TRUE)
	
	tout <- spectrapply(se,
		FUN=function(x, t, ...) t,
		spectra="x", index="t",
		simplify=TRUE)

	expect_equal(spectra(se, "x"), xout)
	expect_equal(featureData(se)$t, tout[,1])

})

test_that("spectrapply - MSImagingArrays", {

	path <- CardinalIO::exampleImzMLFile("processed")
	msa <- readImzML(path, memory=TRUE)

	tic <- spectrapply(msa, sum)

	expect_equal(tic, sapply(intensity(msa), sum))

})

test_that("spectrapply - MSImagingExperiment", {

	path <- CardinalIO::exampleImzMLFile("continuous")
	mse <- readImzML(path, memory=TRUE)

	tic <- spectrapply(mse, sum)

	expect_equal(tic, apply(spectra(mse), 2L, sum))

})
