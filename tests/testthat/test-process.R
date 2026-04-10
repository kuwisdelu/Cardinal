require(testthat)
require(Cardinal)

context("processing")

# .setup_MSImagingArrays <- function(n = 10L, p = 20L)
# {
# 	ns <- sample(p, n, replace=TRUE)
# 	domain <- seq_len(p)
# 	intensity <- lapply(ns, function(ni) rlnorm(ni))
# 	index <- lapply(ns, function(ni) sort(sample(domain, ni)))
# 	a <- SpectraArrays(list(index=index, intensity=intensity))
# 	SpectralImagingArrays(a)
# }

# test_that("normalize", {

# 	set.seed(1)
# 	sa <- .setup_SpectraImagingArrays()

# 	sa_tic <- normalize(sa, method="tic")
# 	sa_tic <- normalize(sa, method="tic")

# 	expect_true(validObject(applyProcessing(sa2)))

# })
