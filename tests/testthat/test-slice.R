require(testthat)
require(Cardinal)

context("slice")

test_that("slice", {

	set.seed(1)
	s <- simulateImage(preset=1, dim=c(10L, 10L), nrun=2,
		representation="centroid")

	mz <- c(564.3, 603.7)
	rs <- slice(s, i=1)
	rs2 <- slice(s, i=1:2)
	rs3 <- slice(s, mz=mz[1L])
	rs4 <- slice(s, mz=mz)

	expect_equal(dim(rs), c(10,10,2))
	expect_equal(dim(rs2), c(2,10,10,2))
	expect_equal(dim(rs3), c(10,10,2))
	expect_equal(dim(rs4), c(2,10,10,2))
	expect_equal(rs, rs2[1L,,,])
	expect_equal(rs, rs3)
	expect_equal(rs2, rs4)
	expect_equal(rs3, rs4[1L,,,])

})
