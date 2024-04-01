require(testthat)
require(Cardinal)

context("SpatialResults")

test_that("SpatialResults and ResultsList", {

	require(datasets)
	fit1 <- lm(mpg ~ disp, data=mtcars)
	fit2 <- lm(mpg ~ disp + wt, data=mtcars)
	fit3 <- lm(mpg ~ disp + hp, data=mtcars)

	n <- 100
	mz <- seq(500, 510, length.out=n)
	mdf <- MassDataFrame(mz=mz, A=seq_len(n), B=rev(seq_len(n)))
	Coord <- expand.grid(x=1:9, y=1:9)
	n <- nrow(Coord)
	pdf <- PositionDataFrame(coord=Coord, A=seq_len(n), B=rev(seq_len(n)))
	sr <- SpatialResults(fit1, featureData=mdf, pixelData=pdf)

	expect_equal(modelData(sr), fit1)
	expect_equal(length(sr), length(fit1))
	expect_equal(names(sr), names(fit1))
	expect_equal(sr[["coefficients"]], fit1[["coefficients"]])
	expect_equal(sr$coefficients, fit1$coefficients)
	expect_equal(coef(sr), coef(fit1))
	expect_equal(resid(sr), resid(fit1))
	expect_equal(fitted(sr), fitted(fit1))
	expect_equal(featureData(sr), mdf)
	expect_equal(featureNames(sr), rownames(mdf))
	expect_equal(pixelData(sr), pdf)
	expect_equal(pixelNames(sr), rownames(pdf))
	expect_equal(coord(sr), coord(pdf))
	expect_equal(coordNames(sr), coordNames(pdf))
	expect_equal(run(sr), run(pdf))
	expect_equal(runNames(sr), runNames(pdf))
	expect_equal(nrun(sr), nrun(pdf))

	rl <- ResultsList(fit1=fit1, fit2=fit2)
	mcols(rl) <- DataFrame(NumParams=c(1, 2))
	rl2 <- ResultsList(fit3=fit3)
	mcols(rl2) <- DataFrame(NumParams=2)
	rl3 <- c(rl, rl2)

	expect_true(validObject(rl))
	expect_is(rl, "ResultsList")
	expect_is(rl3, "ResultsList")
	expect_equal(mcols(rl3), rbind(mcols(rl), mcols(rl2)))

	rl4 <- ResultsList(sr)

	expect_true(validObject(rl4))
	expect_is(rl4, "ResultsList")
	expect_equal(rl4[[1L]], sr)

})

