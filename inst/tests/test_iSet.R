require(testthat)

context("iSet class")

test_that("iSet validity", {
	
	expect_error(new("iSet"))

})

test_that("iSet inheritance and manipulation", {

	MyImageSet <- setClass("MyImageSet", contains="iSet")
	iset1 <- MyImageSet()
	expect_true(validObject(iset1))

	idata <- ImageData(data0=array(1:27, dim=c(Features=3, x=3, y=3)))
	iset2 <- MyImageSet(imageData=idata)
	expect_true(validObject(iset2))

	df <- IAnnotatedDataFrame(data=expand.grid(x=1:3, y=1:3),
		varMetadata=data.frame(labelType=c(x="spatial2d", y="spatial2d")))
	iset3 <- MyImageSet(imageData=idata, pixelData=df)

	iset3$dummy1 <- 1:9
	iset3[["dummy2", labelDescription="test"]] <- iset3$dummy1
	expect_equal(iset3$dummy1, iset3$dummy2)
	expect_identical(varMetadata(iset3)["dummy2","labelDescription"], "test")

	removeClass("MyImageSet")

})

