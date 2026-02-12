require(testthat)
require(Cardinal)

context("SpectralImagingData")

test_that("SpectralImagingData extension", {

	setClass("TestSpectralImagingData",
		contains="SpectralImagingData")

	nx <- 5L
	ny <- 2L
	n <- nx * ny
	arrays <- replicate(n, rlnorm(sample(n, 1L)), simplify=FALSE)
	arrays <- SpectraArrays(list(intensity=arrays))
	pdata <- PositionDataFrame(coord=expand.grid(x=1:nx, y=1:ny))

	s1 <- new("TestSpectralImagingData",
		spectraData=SpectraArrays(),
		elementMetadata=PositionDataFrame())
	s2 <- new("TestSpectralImagingData",
		spectraData=arrays,
		elementMetadata=pdata)

	expect_true(validObject(s1))
	expect_true(validObject(s2))
	expect_equal(spectraData(s1), SpectraArrays())
	expect_equal(spectraData(s2), arrays)
	expect_equal(pixelData(s1), PositionDataFrame())
	expect_equal(pixelData(s2), pdata)

})
