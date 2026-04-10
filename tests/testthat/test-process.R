require(testthat)
require(Cardinal)

context("processing")

.setup_MSImagingArrays <- function(n = 10L, p = 20L)
{
	ns <- sample(p, n, replace=TRUE)
	mzrange <- runif(p, min=100, max=1000)
	intensity <- lapply(ns, function(ni) rlnorm(ni))
	mz <- lapply(ns, function(ni) sort(sample(mzrange, ni)))
	a <- SpectraArrays(list(mz=mz, intensity=intensity))
	MSImagingArrays(a, centroided=FALSE)
}

# tests for workhorse functions are currently in pkg:matter
# to be refactored and moved to pkg:CardinalCore eventually --
# we only check that the infrastructure works here

test_that("normalize", {

	set.seed(1)
	msa <- .setup_MSImagingArrays()
	mzref <- mz(msa)[[1L]][[1L]]

	msa_tic <- normalize(msa, method="tic")
	msa_rms <- normalize(msa, method="rms")
	msa_ref <- normalize(msa, method="reference", ref=mzref)

	expect_true(validObject(applyProcessing(msa_tic)))
	expect_true(validObject(applyProcessing(msa_rms)))
	expect_true(validObject(applyProcessing(msa_ref)))

})
