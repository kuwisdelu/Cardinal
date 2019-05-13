require(testthat)
require(Cardinal)
require(EBImage)

context("AnnotatedImageList class")

test_that("AnnotatedImageList validity", {
	
	expect_true(validObject(new("AnnotatedImageList")))

	x <- Image(rnorm(30*30*3), dim=c(30,30,3), colormode='Color')

	idata <- AnnotatedImageList(x)
	expect_true(validObject(idata))

})

test_that("AnnotatedImageList accessors", {

	x <- Image(rnorm(30*30*3), dim=c(30,30,3), colormode='Color')
	x <- as(x, "AnnotatedImage")

	idata <- AnnotatedImageList(A=x, B=x, C=x)

	expect_equal(idata[[1]], x)
	expect_equal(idata[["A"]], x)
	
	expect_equal(length(idata), 3L)
	expect_equal(dim(idata), c(3L, 3L))
	expect_equal(dims(idata)[,1], c(30L, 30L, 3L))
	
	expect_equal(dim(idata[1,]), c(1L, 3L))
	expect_equal(idata[1,][[1L]], x[,,1L])

	ridata <- rbind(idata, idata)
	expect_equal(dim(ridata), c(6L, 3L))
	expect_equal(dims(ridata)[,1], c(30L, 30L, 3L, 2L))

	idata1 <- AnnotatedImageList(A=x, B=x)
	idata2 <- AnnotatedImageList(C=x, D=x)

	cidata <- cbind(idata1, idata2)
	expect_equal(length(cidata), 4L)
	expect_equal(cidata, combine(idata1, idata2))

})

