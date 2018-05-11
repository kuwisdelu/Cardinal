require(testthat)
require(Cardinal)

context("PositionDataFrame class")

test_that("PositionDataFrame validity", {
	
	expect_true(validObject(new("PositionDataFrame")))

	coord <- expand.grid(x=1:3, y=1:3)
	values <- seq_len(nrow(coord))
	pdata <- PositionDataFrame(coord=coord, values=values)
	expect_true(validObject(pdata))

})

test_that("PositionDataFrame accessors", {

	coord <- expand.grid(x=1:3, y=1:3)
	values <- seq_len(nrow(coord))
	pdata <- PositionDataFrame(coord=coord, values=values)

	expect_equal(coord(pdata), DataFrame(coord))
	expect_equal(coord(pdata)$x, coord$x)
	expect_equal(coord(pdata)$y, coord$y)
	
	expect_true(gridded(pdata))
	expect_equal(resolution(pdata), c(x=1, y=1))
	expect_equal(dims(pdata), c(x=3, y=3))

	coord(pdata)$x <- coord(pdata)$x + runif(nrow(pdata)) / 10
	coord(pdata)$y <- coord(pdata)$y + runif(nrow(pdata)) / 10

	expect_false(gridded(pdata))
	expect_equal(resolution(pdata), c(x=NA_real_, y=NA_real_))
	expect_equal(dims(pdata), c(x=NA_real_, y=NA_real_))

	expect_equal(length(pdata), 1)
	expect_equal(dim(pdata), c(nrow(coord), 1))
	expect_equal(nrow(pdata), nrow(coord))
	expect_equal(ncol(pdata), 1)
	expect_equal(names(pdata), "values")

})

test_that("PositionDataFrame subsetting", {

	coord <- expand.grid(x=1:3, y=1:3)
	values <- seq_len(nrow(coord))
	pdata <- PositionDataFrame(coord=coord, values=values)
	mcols(pdata) <- DataFrame(meta1=1, meta2=2)
	
	pdata1 <- pdata[1:5,,drop=FALSE]
	expect_equal(pdata1$values, values[1:5])
	expect_equal(coord(pdata1), DataFrame(coord)[1:5,,drop=FALSE])
	expect_equal(mcols(pdata1), mcols(pdata))

	pdata2 <- pdata[,1,drop=FALSE]
	expect_equal(pdata2$values, values)
	expect_equal(coord(pdata2), DataFrame(coord))
	expect_equal(mcols(pdata2), mcols(pdata)[1])

	pdata3 <- pdata[1:5,1,drop=FALSE]
	expect_equal(pdata3$values, values[1:5])
	expect_equal(coord(pdata3), DataFrame(coord)[1:5,,drop=FALSE])
	expect_equal(mcols(pdata3), mcols(pdata)[1])

})

test_that("PositionDataFrame binding", {

	coord <- expand.grid(x=1:6, y=1:3)
	
	coord1 <- coord[1:9,]
	values <- seq_len(nrow(coord1))
	pdata1 <- PositionDataFrame(coord=coord1, value=values)
	mcols(pdata1) <- DataFrame(meta1=1, meta2=2)

	coord2 <- coord[10:18,]
	values <- seq_len(nrow(coord2))
	pdata2 <- PositionDataFrame(coord=coord2, value=values)
	mcols(pdata2) <- DataFrame(meta1=1, meta2=2)

	expect_error(cbind(pdata1, pdata2))

	pdataB <- cbind(pdata1, pdata1)
	
	expect_true(validObject(pdataB))
	expect_equal(ncol(pdataB), 2)
	expect_equal(mcols(pdataB), rbind(mcols(pdata1), mcols(pdata1)))

	# expect_error(rbind(pdata1, pdata1)) # allow duplicate coords?

	pdataB <- rbind(pdata1, pdata2)
	
	expect_true(validObject(pdataB))
	expect_equal(nrow(pdataB), nrow(coord))
	expect_equal(mcols(pdataB), mcols(pdata1))
	expect_equal(mcols(pdataB), mcols(pdata2))

})
