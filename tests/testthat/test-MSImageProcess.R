require(testthat)

context("MSImageProcess class")

test_that("MSImageProcess validity", {
	
	expect_true(validObject(new("MSImageProcess")))

})

test_that("MSImageProcess combine", {

	proc1 <- new("MSImageProcess")
	proc2 <- new("MSImageProcess")
	proc12 <- combine(proc1, proc2)
	expect_true(validObject(proc12))	

})
