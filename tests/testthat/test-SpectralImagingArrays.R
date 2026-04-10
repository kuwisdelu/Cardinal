require(testthat)
require(Cardinal)

context("SpectralImagingArrays")

.setup_SpectraArrays <- function(n = 10L, p = 20L)
{
	ns <- sample(p, n, replace=TRUE)
	domain <- seq_len(p)
	intensity <- lapply(ns, function(ni) rlnorm(ni))
	index <- lapply(ns, function(ni) sort(sample(domain, ni)))
	SpectraArrays(list(index=index, intensity=intensity))
}

test_that("SpectralImagingArrays validity ok", {

	expect_true(validObject(SpectralImagingArrays()))
	expect_true(validObject(SpectralImagingArrays(numeric(0))))

	n <- 10L
	a1 <- lapply(seq_len(n), seq_len)
	sdata1 <- SpectraArrays(list(intensity=a1))
	pdata1 <- PositionDataFrame(coord=list(x=seq_len(n + 1L), y=1L))

	expect_error(SpectralImagingArrays(sdata1, pixelData=pdata1))
	expect_error(SpectralImagingArrays(sdata1, continuous=TRUE))

	a2 <- replicate(n, seq_len(n), simplify=FALSE)
	sdata2 <- SpectraArrays(list(intensity=a2))
	sdata3 <- SpectraArrays(list(intensity=a1, mismatched_intensity=a2))

	expect_true(validObject(SpectralImagingArrays(sdata2, continuous=TRUE)))
	expect_error(SpectralImagingArrays(sdata3))

})

test_that("SpectralImagingArrays accessors ok", {

	sa1 <- SpectralImagingArrays(
		spectraData=SpectraArrays(),
		pixelData=PositionDataFrame(),
		centroided=NA)

	set.seed(1)
	nx <- 5L
	ny <- 2L
	n <- nx * ny
	sdata2 <- .setup_SpectraArrays(n)
	pdata2 <- PositionDataFrame(
		coord=expand.grid(x=1:nx, y=1:ny),
		trt=sample(c("A", "B"), n, replace=TRUE),
		row.names=letters[seq_len(n)])	
	sa2 <- SpectralImagingArrays(
		spectraData=sdata2,
		pixelData=pdata2,
		centroided=NA)

	expect_true(validObject(sa1))
	expect_true(validObject(sa2))

	expect_null(dim(sa1))
	expect_null(dim(sa2))

	expect_length(sa1, 0L)
	expect_length(sa2, n)

	expect_equal(lengths(sa1), integer(0L))
	expect_equal(lengths(sa2), lengths(sdata2[[1L]]))

	expect_equal(spectraData(sa1), SpectraArrays())
	expect_equal(spectraData(sa2), sdata2)

	expect_equal(spectraNames(sa1), character())
	expect_equal(spectraNames(sa2), names(sdata2))

	expect_error(spectra(sa1))
	expect_equal(spectra(sa2, 1L), sdata2[[1L]])

	expect_error(spectra(sa1))
	expect_equal(spectra(sa2, 2L), sdata2[[2L]])

	expect_error(spectra(sa1))
	expect_equal(spectra(sa2, "index"), sdata2[["index"]])

	expect_error(spectra(sa1))
	expect_equal(spectra(sa2, "intensity"), sdata2[["intensity"]])

	expect_equal(pixelData(sa1), PositionDataFrame())
	expect_equal(pixelData(sa2), pdata2)

	expect_equal(pixelNames(sa1), NULL)
	expect_equal(pixelNames(sa2), row.names(pdata2))

	expect_equal(pixelVariables(sa1), names(PositionDataFrame()))
	expect_equal(pixelVariables(sa2), names(pdata2))

	expect_equal(coord(sa2), coord(pdata2))
	expect_equal(coordNames(sa2), coordNames(pdata2))
	
	expect_equal(run(sa2), run(pdata2))
	expect_equal(runNames(sa2), runNames(pdata2))
	expect_equal(nrun(sa2), nrun(pdata2))
	expect_equal(is3D(sa2), is3D(pdata2))

	expect_equal(centroided(sa1), NA)
	expect_equal(centroided(sa2), NA)

	expect_length(sa1, 0L)
	expect_length(sa2, n)

	expect_equal(sa2$trt, pdata2$trt)
	expect_equal(sa2[["trt"]], pdata2[["trt"]])

})

test_that("SpectralImagingArrays combine ok", {

	set.seed(1)
	n <- 10L
	
	a1 <- .setup_SpectraArrays(n)
	a2 <- .setup_SpectraArrays(n)
	pdata1 <- PositionDataFrame(run=rep.int("runA", n))
	pdata2 <- PositionDataFrame(run=rep.int("runB", n))
	
	sa1 <- SpectralImagingArrays(a1, pixelData=pdata1)
	sa2 <- SpectralImagingArrays(a2, pixelData=pdata2)

	sa3 <- combine(sa1, sa2)

	expect_equal(spectra(sa3, 1L), c(spectra(sa1, 1L), spectra(sa2, 1L)))
	expect_equal(spectra(sa3, 2L), c(spectra(sa1, 2L), spectra(sa2, 2L)))
	expect_equal(pData(sa3), rbind(pData(sa1), pData(sa2)))
	expect_equal(sa3, c(sa1, sa2))

})

test_that("SpectralImagingArrays processing ok", {

	set.seed(1)
	n <- 25L
	a1 <- .setup_SpectraArrays(n)
	sa1 <- SpectralImagingArrays(a1)

	expect_equal(processingChunkSize(sa1), NA_integer_)

	chunksize <- 10L
	processingChunkSize(sa1) <- chunksize

	expect_equal(processingChunkSize(sa1), chunksize)
	expect_length(processingChunkFactor(sa1), length(sa1))
	expect_true(is.factor(processingChunkFactor(sa1)))

	NORM <- function(x, tic = 1, ...) {
		x$intensity <- tic * x$intensity / sum(x$intensity)
		x
	}
	LAB_NORM <- "intensity normalization"
	
	x1 <- spectrapply(sa1, identity)
	x2 <- lapply(x1, NORM)
	sa2 <- addProcessing(sa1, NORM, id=LAB_NORM)
	sa2out <- applyProcessing(sa2)

	expect_length(processingData(sa2), 1L)
	expect_equal(names(processingData(sa2)), LAB_NORM)
	expect_identical(pixelData(sa2), pixelData(sa1))
	expect_identical(dropProcessing(sa2), sa1)
	expect_identical(
		spectra(sa2out, "intensity"),
		lapply(x2, "[[", "intensity"))

	LOG2P1 <- function(x, ..) {
		x$log2intensity <- log2(x$intensity + 1)
		x
	}
	LAB_LOG2P1 <- "log2 transformation"
	
	x3 <- lapply(lapply(x1, NORM), LOG2P1)
	sa3 <- addProcessing(sa2, LOG2P1, id=LAB_LOG2P1)
	sa3out <- applyProcessing(sa3)

	expect_length(processingData(sa3), 2L)
	expect_equal(names(processingData(sa3)), c(LAB_NORM, LAB_LOG2P1))
	expect_identical(pixelData(sa3), pixelData(sa1))
	expect_identical(dropProcessing(sa3), sa1)
	expect_identical(
		spectra(sa3out, "log2intensity"),
		lapply(x3, "[[", "log2intensity"))

	ADD <- function(x, b, ...) {
		x$intensity <- x$intensity + b
		x
	}
	LAB_ADD <- "add pixel variable"
	
	set.seed(1)
	sa1$b <- runif(length(sa1))
	sa4 <- addProcessing(sa1, ADD, pixelVariables="b", id=LAB_ADD)
	sa4out <- applyProcessing(sa4)

	expect_identical(
		spectra(sa4out, "intensity"),
		Map("+", spectra(sa1, "intensity"), sa1$b))

})

test_that("SpectralImagingArrays pixels", {

	expect_true(validObject(SpectralImagingArrays()))
	expect_true(validObject(SpectralImagingArrays(numeric(0))))

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

	expect_setequal(pixels(sa, 1:10), 1:10)
	expect_setequal(pixels(sa, diagnosis == "yes"), 1:5)
	expect_setequal(pixels(sa, diagnosis == "no"), 6:10)
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

