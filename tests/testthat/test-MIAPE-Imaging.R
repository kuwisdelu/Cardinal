require(testthat)

context("MIAPE-Imaging class")

test_that("MIAPE-Imaging validity", {
	
	expect_true(validObject(new("MIAPE-Imaging")))

})

test_that("MIAPE-Imaging combine", {

	miape1 <- new("MIAPE-Imaging")
	miape2 <- new("MIAPE-Imaging")
	miape12 <- combine(miape1, miape2)
	expect_true(validObject(miape12))	

})
