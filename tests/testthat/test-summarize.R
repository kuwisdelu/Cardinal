require(testthat)
require(Cardinal)

context("summarize")

test_that("summarizeFeatures", {

	path <- CardinalIO::exampleImzMLFile("continuous")
	mse <- readImzML(path, memory=TRUE)
	g <- makeFactor(A=mse$y==1, B=mse$y==2, C=mse$y==3)

	mse <- summarizeFeatures(mse)
	mse <- summarizeFeatures(mse, groups=g)

	expect_equal(fData(mse)$mean, rowMeans(mse))
	expect_equal(fData(mse)$A.mean, rowMeans(mse[,g=="A"]))
	expect_equal(fData(mse)$B.mean, rowMeans(mse[,g=="B"]))
	expect_equal(fData(mse)$C.mean, rowMeans(mse[,g=="C"]))

	path2 <- CardinalIO::exampleImzMLFile("processed")
	mse2 <- readImzML(path2, memory=TRUE)

	mse2 <- summarizeFeatures(mse2)
	mse2 <- summarizeFeatures(mse2, groups=g)

	expect_equal(fData(mse2)$mean, rowMeans(mse2))
	expect_equal(fData(mse2)$A.mean, rowMeans(mse2[,g=="A"]))
	expect_equal(fData(mse2)$B.mean, rowMeans(mse2[,g=="B"]))
	expect_equal(fData(mse2)$C.mean, rowMeans(mse2[,g=="C"]))

})

test_that("summarizePixels", {

	path <- CardinalIO::exampleImzMLFile("continuous")
	mse <- readImzML(path, memory=TRUE)
	g <- makeFactor(light=mz(mse) < 400, heavy=mz(mse) >= 400)

	mse <- summarizePixels(mse)
	mse <- summarizePixels(mse, "sum", groups=g)

	expect_equal(pData(mse)$tic, colSums(mse))
	expect_equivalent(pData(mse)$light.sum, colSums(mse[g=="light",]))
	expect_equivalent(pData(mse)$heavy.sum, colSums(mse[g=="heavy",]))

	path2 <- CardinalIO::exampleImzMLFile("processed")
	mse2 <- readImzML(path2, memory=TRUE)

	mse2 <- summarizePixels(mse2)

	expect_equal(pData(mse2)$tic, colSums(mse2))

})

