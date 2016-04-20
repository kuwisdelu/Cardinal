require(testthat)

context("IAnnotatedDataFrame class")

test_that("IAnnotatedDataFrame validity", {
	
	expect_true(validObject(new("IAnnotatedDataFrame")))
	expect_true(validObject(IAnnotatedDataFrame()))

	df <- IAnnotatedDataFrame()
	expect_that("sample" %in% varLabels(df), is_true())
	expect_that("labelType" %in% names(varMetadata(df)), is_true())
	expect_that(varMetadata(df)["sample","labelType"] == "sample", is_true())

})

test_that("IAnnotatedDataFrame accessors and assignment", {

	coord1 <- expand.grid(x=1:3, y=1:3)
	df <- IAnnotatedDataFrame(coord1, data.frame(labelType=c("dim", "dim")))
	expect_true(all(coord(df) == coord1))
	expect_equal(names(coord(df)), names(coord1))
	expect_equal(coordLabels(df), names(coord1))

	coordLabels(df) <- c("x1", "x2")
	expect_equal(coordLabels(df), c("x1", "x2"))

	coord2 <- expand.grid(x=3:1, y=3:1)
	coord(df) <- coord2
	expect_true(all(coord(df) == coord2))
	expect_equal(coordLabels(df), c("x1", "x2"))

	expect_equal(sampleNames(df), "1")
	expect_error(df$sample <- rep(1:3, each=3))
	df[["sample"]] <- factor(rep(c("a", "b", "c"), each=3))
	expect_true(all(c("a", "b", "c") %in% sampleNames(df)))

	sampleNames(df) <- c("a1", "a2", "a3", "a4")
	expect_true(all(c("a1", "a2", "a3", "a4") %in% levels(df[["sample"]])))

	expect_equal(pixelNames(df), paste(1:9))
	pixelNames(df) <- paste("p", 1:9, sep="")
	expect_equal(pixelNames(df), paste("p", 1:9, sep=""))

})

test_that("IAnnotatedDataFrame combine", {

	df1 <- IAnnotatedDataFrame(data.frame(sample="sample 1", row.names="1"))
	df2 <- IAnnotatedDataFrame(data.frame(sample="sample 2", row.names="2"))

	df3 <- combine(df1, df2)
	expect_true(validObject(df3))
	expect_equal(levels(df3$sample), c("sample 1", "sample 2"))

})
