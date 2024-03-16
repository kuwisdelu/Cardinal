require(testthat)
require(Cardinal)

context("SpectralImagingArrays")

test_that("SpectralImagingArrays accessors", {

	sa <- SpectralImagingArrays()

	expect_true(validObject(sa))

	sa <- SpectralImagingArrays(numeric(0))

	expect_true(validObject(sa))

	set.seed(1)
	n <- 10
	i <- rep(list(1:n), n)
	a <- replicate(n, rlnorm(n), simplify=FALSE)
	s <- SpectraArrays(list(index=i, intensity=a))
	pdata <- PositionDataFrame(
		coord=expand.grid(x=1:5, y=1:2),
		diagnosis=rep(c("yes", "no"), each=5))
	pdata2 <- PositionDataFrame(
		coord=expand.grid(x=1:5, y=3:4),
		diagnosis=rep(c("yes", "no"), each=5))
	sa <- SpectralImagingArrays(s, pixelData=pdata)

	expect_true(validObject(sa))
	expect_length(sa, n)
	expect_null(dim(sa))
	
	expect_equal(spectraData(sa), s)
	expect_equal(pixelData(sa), pdata)
	
	expect_equal(spectra(sa, 1L), s[[1L]])
	expect_equal(spectra(sa, 2L), s[[2L]])
	expect_equal(pData(sa), pdata)

	expect_equal(coord(sa), coord(pdata))
	expect_equal(run(sa), run(pdata))
	expect_equal(pData(sa)$diagnosis, pdata$diagnosis)

	j <- 1
	
	expect_setequal(pixels(sa, j), 1)
	expect_setequal(pixels(sa, 1:10), 1:10)
	expect_setequal(pixels(sa, diagnosis == "yes"), 1:5)
	expect_setequal(pixels(sa, diagnosis == "no"), 6:10)
	expect_setequal(pixels(sa, x > 1, y > j), c(7, 8, 9, 10))
	expect_setequal(pixels(sa, coord=c(x=3, y=1)), 3)
	expect_setequal(pixels(sa, coord=c(x=3, y=j)), 3)
	expect_setequal(pixels(sa, run="run0"), 1:10)
	expect_setequal(pixels(sa, run=j), 1:10)

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
		diagnosis=rep(c("yes", "no"), each=5))
	pdata2 <- PositionDataFrame(
		coord=expand.grid(x=1:5, y=3:4),
		diagnosis=rep(c("yes", "no"), each=5))
	sa <- SpectralImagingArrays(s, pixelData=pdata)
	sa2 <- SpectralImagingArrays(s, pixelData=pdata2)

	sa3 <- c(sa, sa2)

	expect_equal(spectra(sa3, 1L), c(spectra(sa, 1L), spectra(sa2, 1L)))
	expect_equal(spectra(sa3, 2L), c(spectra(sa, 2L), spectra(sa2, 2L)))
	expect_equal(pData(sa3), rbind(pData(sa), pData(sa2)))

})
