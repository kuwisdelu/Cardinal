require(testthat)
require(Cardinal)

context("ResultsList")

test_that("ResultsList and SpatialResultsList", {

	require(datasets)
	fit1 <- lm(mpg ~ disp, data=mtcars)
	fit2 <- lm(mpg ~ disp + wt, data=mtcars)
	fit3 <- lm(mpg ~ disp + hp, data=mtcars)

	rl <- ResultsList(fit1=fit1, fit2=fit2)
	mcols(rl) <- DataFrame(NumParams=c(1, 2))
	rl2 <- ResultsList(fit3=fit3)
	modelData(rl2) <- DataFrame(NumParams=2)
	rl3 <- c(rl, rl2)

	expect_true(validObject(rl))
	expect_is(rl, "ResultsList")
	expect_is(rl3, "ResultsList")
	expect_equal(mcols(rl3), rbind(mcols(rl), mcols(rl2)))
	expect_equal(modelData(rl), mcols(rl))
	expect_equal(resultData(rl, "coef"), lapply(rl, coef))
	expect_equal(resultNames(rl), names(rl[[1L]]))

	n <- 100
	mz <- seq(500, 510, length.out=n)
	mdf <- MassDataFrame(mz=mz, A=seq_len(n), B=rev(seq_len(n)))
	Coord <- expand.grid(x=1:9, y=1:9)
	n <- nrow(Coord)
	pdf <- PositionDataFrame(coord=Coord, A=seq_len(n), B=rev(seq_len(n)))
	srl <- ResultsList(fit1, fit2, featureData=mdf, pixelData=pdf)
	modelData(srl) <- DataFrame(NumParams=c(1, 2))
	srl2 <- ResultsList(fit3=fit3, featureData=mdf, pixelData=pdf)
	modelData(srl2) <- DataFrame(NumParams=2)
	srl3 <- c(srl, srl2)

	expect_equal(featureData(srl), mdf)
	expect_equal(featureNames(srl), rownames(mdf))
	expect_equal(pixelData(srl), pdf)
	expect_equal(pixelNames(srl), rownames(pdf))
	expect_equal(coord(srl), coord(pdf))
	expect_equal(coordNames(srl), coordNames(pdf))
	expect_equal(run(srl), run(pdf))
	expect_equal(runNames(srl), runNames(pdf))
	expect_equal(nrun(srl), nrun(pdf))
	expect_equal(featureData(srl), featureData(srl3))
	expect_equal(pixelData(srl), pixelData(srl3))

})

