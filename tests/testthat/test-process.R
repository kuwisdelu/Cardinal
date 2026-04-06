require(testthat)
require(Cardinal)

context("processing")

test_that("addProcessing", {

	set.seed(1)
	n <- 10L
	arrays <- replicate(n, rlnorm(sample(n, 1L)), simplify=FALSE)
	sarrays <- SpectraArrays(list(intensity=arrays))
	sa <- SpectralImagingArrays(spectraData=sarrays)

	FUN1 <- function(x) {
		list(intensity=x$intensity / sum(x$intensity))
	}
	LAB1 <- "normalization"
	sa1 <- addProcessing(sa, FUN1, label=LAB1)

	expect_length(processingData(sa1), 1L)
	expect_equal(names(processingData(sa1)), LAB1)
	expect_identical(dropProcessing(sa1), sa)

	FUN2 <- function(x, threshold) x[x > threshold]
	LAB2 <- "thresholding"
	sa2 <- sa |>
		addProcessing(FUN1, label=LAB1) |>
		addProcessing(FUN2, threshold=0, label=LAB2)

	expect_length(processingData(sa2), 2L)
	expect_equal(names(processingData(sa2)), c(LAB1, LAB2))
	expect_identical(dropProcessing(sa2), sa)

	FUN3 <- function(x, tic) tic * x / sum(x)
	LAB3 <- "custom normalization"
	pixelData(sa)[["tic"]] <- vapply(arrays, sum, numeric(1L))
	sa3 <- addProcessing(sa, FUN3, pixelVariables="tic", label=LAB3)

	expect_length(processingData(sa3), 1L)
	expect_equal(names(processingData(sa3)), LAB3)
	expect_identical(dropProcessing(sa3), sa)

	expect_error(addProcessing(sa, FUN3, pixelVariables="invalid"))

})
