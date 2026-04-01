require(testthat)
require(Cardinal)

context("SpectralImagingArrays")

test_that("SpectralImagingArrays accessors", {

	expect_true(validObject(SpectralImagingArrays()))
	expect_true(validObject(SpectralImagingArrays(numeric(0))))

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

	sa1 <- SpectralImagingArrays(
		spectraData=SpectraArrays(),
		pixelData=PositionDataFrame(),
		centroided=NA)
	sa2 <- SpectralImagingArrays(
		spectraData=sarrays,
		pixelData=pdata,
		centroided=NA)

	expect_true(validObject(sa1))
	expect_true(validObject(sa2))

	expect_equal(spectraData(sa1), SpectraArrays())
	expect_equal(spectraData(sa2), sarrays)

	expect_equal(spectraNames(sa1), character())
	expect_equal(spectraNames(sa2), "intensity")

	expect_error(spectra(sa1))
	expect_equal(spectra(sa2), arrays)

	expect_error(spectra(sa1))
	expect_equal(spectra(sa2), arrays)

	expect_equal(pixelData(sa1), PositionDataFrame())
	expect_equal(pixelData(sa2), pdata)

	expect_equal(pixelNames(sa1), NULL)
	expect_equal(pixelNames(sa2), row.names(pdata))

	expect_equal(pixelVariables(sa1), names(PositionDataFrame()))
	expect_equal(pixelVariables(sa2), names(pdata))

	expect_equal(coord(sa2), coord(pdata))
	expect_equal(coordNames(sa2), coordNames(pdata))
	
	expect_equal(run(sa2), run(pdata))
	expect_equal(runNames(sa2), runNames(pdata))
	expect_equal(nrun(sa2), nrun(pdata))
	expect_equal(is3D(sa2), is3D(pdata))

	expect_equal(centroided(sa1), NA)
	expect_equal(centroided(sa2), NA)

	expect_length(sa1, 0L)
	expect_length(sa2, n)

	expect_equal(sa2$trt, pdata$trt)
	expect_equal(sa2[["trt"]], pdata[["trt"]])

})

test_that("SpectralImagingArrays processing", {

	set.seed(1)
	n <- 999L
	arrays <- replicate(n, rlnorm(sample(n, 1L)), simplify=FALSE)
	index <- lapply(arrays, seq_along)
	sarrays <- SpectraArrays(list(index=index, intensity=arrays))

	sa <- SpectralImagingArrays(sarrays)

	expect_equal(processingChunkSize(sa), NA_integer_)

	chunksize <- 100L
	processingChunkSize(sa) <- chunksize

	expect_equal(processingChunkSize(sa), chunksize)
	expect_true(is.factor(processingChunkFactor(sa)))
	expect_length(processingChunkFactor(sa), length(sa))

})

test_that("SpectralImagingArrays pixels", {

	expect_true(validObject(SpectralImagingArrays()))
	expect_true(validObject(SpectralImagingArrays(numeric(0))))

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

	sa <- SpectralImagingArrays(
		spectraData=sarrays,
		pixelData=pdata)

	expect_setequal(pixels(sa, 1:10), 1:10)
	expect_setequal(pixels(sa, trt == "A"), 1:5)
	expect_setequal(pixels(sa, trt == "B"), 6:10)
	expect_setequal(pixels(sa, coord=c(x=3, y=1)), 3)
	expect_setequal(pixels(sa, run="run0"), 1:10)

	j <- 1
	
	expect_setequal(pixels(sa, j), 1)
	expect_setequal(pixels(sa, x > 1, y > j), c(7, 8, 9, 10))
	expect_setequal(pixels(sa, coord=c(x=3, y=j)), 3)

	expect_equal(subset(sa, j), sa[j])
	expect_equal(subset(sa, x > 1 & y > j), sa[c(7, 8, 9, 10)])
	expect_equal(subset(sa, x == 3 & y == j), sa[3])

	pixelData(sa) <- pdata2

	expect_true(validObject(sa))
	expect_equal(pixelData(sa), pdata2)

	coord(sa) <- coord(pdata)
	run(sa) <- run(pdata)

	expect_equal(coord(sa), coord(pdata))
	expect_equal(run(sa), run(pdata))

	sa2 <- sa[2:9]

	expect_true(validObject(sa2))
	expect_equal(length(sa2), 8L)
	expect_equal(spectra(sa2, 1L), spectra(sa, 1L)[2:9])
	expect_equal(spectra(sa2, 2L), spectra(sa, 2L)[2:9])
	expect_equal(pixelData(sa2), pixelData(sa)[2:9,])

})

test_that("SpectralImagingArrays combine", {

	set.seed(1)
	n <- 10
	i <- rep(list(1:n), n)
	a <- replicate(n, rlnorm(n), simplify=FALSE)
	s <- SpectraArrays(list(index=i, intensity=a))
	pdata <- PositionDataFrame(
		coord=expand.grid(x=1:5, y=1:2),
		trt=rep(c("A", "B"), each=5))
	pdata2 <- PositionDataFrame(
		coord=expand.grid(x=1:5, y=3:4),
		trt=rep(c("A", "B"), each=5))
	sa <- SpectralImagingArrays(s, pixelData=pdata)
	sa2 <- SpectralImagingArrays(s, pixelData=pdata2)

	sa3 <- c(sa, sa2)

	expect_equal(spectra(sa3, 1L), c(spectra(sa, 1L), spectra(sa2, 1L)))
	expect_equal(spectra(sa3, 2L), c(spectra(sa, 2L), spectra(sa2, 2L)))
	expect_equal(pData(sa3), rbind(pData(sa), pData(sa2)))

})
