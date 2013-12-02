require(testthat)

context("iSet class")

test_that("iSet validity", {
	
	expect_error(new("iSet"))

})

test_that("iSet inheritance", {

	MyImageSet <- setClass("MyImageSet", contains="iSet")
	iset1 <- MyImageSet()
	expect_true(validObject(iset1))

	idata <- ImageData(data0=array(1:27, dim=c(Features=3,x=3,y=3)))
	iset2 <- MyImageSet(imageData=idata)
	expect_true(validObject(iset2))

})

