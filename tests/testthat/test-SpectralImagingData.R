require(testthat)
require(Cardinal)

context("SpectralImagingData")

test_that("SpectralImagingData extension", {

	setClass("TestSpectralImagingData",
		contains="SpectralImagingData")

	set.seed(1)
	nx <- 5L
	ny <- 2L
	n <- nx * ny
	arrays <- replicate(n, rlnorm(sample(n, 1L)), simplify=FALSE)
	sarrays <- SpectraArrays(list(intensity=arrays))
	pdata <- PositionDataFrame(
		coord=expand.grid(x=1:nx, y=1:ny),
		trt=sample(c("A", "B"), n, replace=TRUE),
		row.names=letters[seq_len(n)])

	s1 <- new("TestSpectralImagingData",
		spectraData=SpectraArrays(),
		elementMetadata=PositionDataFrame(),
		centroided=NA)
	s2 <- new("TestSpectralImagingData",
		spectraData=sarrays,
		elementMetadata=pdata,
		centroided=NA)

	expect_true(validObject(s1))
	expect_true(validObject(s2))

	expect_equal(spectraData(s1), SpectraArrays())
	expect_equal(spectraData(s2), sarrays)

	expect_equal(spectraNames(s1), character())
	expect_equal(spectraNames(s2), "intensity")

	expect_error(spectra(s1))
	expect_equal(spectra(s2), arrays)

	expect_error(spectra(s1))
	expect_equal(spectra(s2), arrays)

	expect_equal(pixelData(s1), PositionDataFrame())
	expect_equal(pixelData(s2), pdata)

	expect_equal(pixelNames(s1), NULL)
	expect_equal(pixelNames(s2), row.names(pdata))

	expect_equal(coord(s2), coord(pdata))
	expect_equal(coordNames(s2), coordNames(pdata))
	
	expect_equal(run(s2), run(pdata))
	expect_equal(runNames(s2), runNames(pdata))
	expect_equal(nrun(s2), nrun(pdata))
	expect_equal(is3D(s2), is3D(pdata))

	expect_equal(centroided(s1), NA)
	expect_equal(centroided(s2), NA)

	expect_length(s1, 0L)
	expect_length(s2, n)

	expect_equal(s2$trt, pdata$trt)
	expect_equal(s2[["trt"]], pdata[["trt"]])

})
