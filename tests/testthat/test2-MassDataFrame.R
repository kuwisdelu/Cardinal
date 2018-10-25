require(testthat)
require(Cardinal)

context("MassDataFrame class")

test_that("MassDataFrame validity", {
	
	expect_true(validObject(new("MassDataFrame")))

	mz <- seq(from=100, to=150, by=1.5)
	values <- seq_len(length(mz))
	fdata <- MassDataFrame(mz=mz, values=values)
	expect_true(validObject(fdata))

})

test_that("MassDataFrame accessors", {

	mz <- seq(from=100, to=150, by=1.5)
	values <- seq_len(length(mz))
	fdata <- MassDataFrame(mz=mz, values=values)

	expect_equal(mz(fdata), mz)
	expect_equal(resolution(fdata), c(mz = 1.5))

	mz2 <- seq(from=100, to=145, length.out=length(mz))
	mz(fdata) <- mz2
	
	expect_equal(mz(fdata), mz2)
	expect_equal(resolution(fdata), c(mz=diff(mz2)[1]))

	expect_equal(length(fdata), 1)
	expect_equal(dim(fdata), c(length(mz), 1))
	expect_equal(nrow(fdata), length(mz))
	expect_equal(ncol(fdata), 1)
	expect_equal(names(fdata), "values")

})

test_that("MassDataFrame subsetting", {

	mz <- seq(from=100, to=150, by=1.5)
	values <- seq_len(length(mz))
	fdata <- MassDataFrame(mz=mz, values=values)
	mcols(fdata) <- DataFrame(meta1=1, meta2=2)
	
	fdata1 <- fdata[1:5,,drop=FALSE]
	expect_equal(fdata1$values, values[1:5])
	expect_equal(mz(fdata1), mz[1:5])
	expect_equal(mcols(fdata1), mcols(fdata))

	fdata2 <- fdata[,1,drop=FALSE]
	expect_equal(fdata2$values, values)
	expect_equal(mz(fdata2), mz)
	expect_equal(mcols(fdata2), mcols(fdata)[1])

	fdata3 <- fdata[1:5,1,drop=FALSE]
	expect_equal(fdata3$values, values[1:5])
	expect_equal(mz(fdata3), mz[1:5])
	expect_equal(mcols(fdata3), mcols(fdata)[1])

})

test_that("MassDataFrame binding", {

	mz <- seq(from=100, to=150, by=1.5)
	
	mz1 <- mz[1:10]
	values <- seq_len(length(mz1))
	fdata1 <- MassDataFrame(mz=mz1, values=values)
	mcols(fdata1) <- DataFrame(meta1=1, meta2=2)

	mz2 <- mz[11:20]
	values <- seq_len(length(mz2))
	fdata2 <- MassDataFrame(mz=mz2, values=values)
	mcols(fdata2) <- DataFrame(meta1=1, meta2=2)

	expect_error(cbind(fdata1, fdata2))

	fdataB <- cbind(fdata1, fdata1)
	
	expect_true(validObject(fdataB))
	expect_equal(ncol(fdataB), 2)
	expect_equal(mcols(fdataB), rbind(mcols(fdata1), mcols(fdata1)))

	expect_error(rbind(fdata1, fdata1))

	fdataB <- rbind(fdata1, fdata2)
	
	expect_true(validObject(fdataB))
	expect_equal(nrow(fdataB), length(mz1) + length(mz2))
	expect_equal(mcols(fdataB), mcols(fdata1))
	expect_equal(mcols(fdataB), mcols(fdata2))

})
