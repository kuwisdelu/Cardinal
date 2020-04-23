require(testthat)
require(Cardinal)

context("manip")

xn <- 15
yn <- 10

pdata <- PositionDataFrame(expand.grid(x=1:xn, y=1:yn),
	run=factor("sample1"), vals=rnorm(xn*yn),
	cond=factor(c(rep("a", 50), rep("b", 100))))

pdata2 <- PositionDataFrame(expand.grid(x=1:xn, y=1:yn),
	run=factor("sample2"), vals=rnorm(xn*yn),
	cond=factor(c(rep("a", 100), rep("b", 50))))

pdata3 <- rbind(pdata, pdata2)

fdata <- MassDataFrame(mz(from=100, to=102.4, by=200))

s <- matrix(nrow=nrow(fdata), ncol=nrow(pdata3))
s[] <- rnorm(prod(dim(s)))
data <- ImageList(list(spectra1=s, spectra2=s))

msdata <- MSImagingExperiment(data, fdata, pdata3)

test_that("subset", {

	i <- 1:10

	tmp1 <- subset(msdata, mz > 101)

	tmp2 <- subsetFeatures(msdata, mz > 101)

	tmp3 <- msdata[mz(msdata) > 101,]

	expect_equal(tmp1, tmp3)

	expect_equal(tmp2, tmp3)

	tmp1 <- subset(msdata, i)

	tmp2 <- subsetFeatures(msdata, i)

	tmp3 <- msdata[i,]

	expect_equal(tmp1, tmp3)

	expect_equal(tmp2, tmp3)

	tmp1 <- subset(msdata, select=x > 5)

	tmp2 <- subsetPixels(msdata, x > 5)

	tmp3 <- msdata[,coord(msdata)$x > 5]

	expect_equal(tmp1, tmp3)

	expect_equal(tmp2, tmp3)

	tmp1 <- subset(msdata, select=i)

	tmp2 <- subsetPixels(msdata, i)

	tmp3 <- msdata[,i]

	expect_equal(tmp1, tmp3)

	expect_equal(tmp2, tmp3)

	tmp1 <- msdata %>% subsetPixels(i) %>% subsetFeatures(i)

	tmp2 <- msdata %>% subsetFeatures(i) %>% subsetPixels(i)

	tmp3 <- msdata[i,i]

	expect_equal(tmp1, tmp3)

	expect_equal(tmp2, tmp3)

})

test_that("summarize (function)", {

	tmp1 <- summarizeFeatures(msdata, FUN=mean)

	tmp2 <- summarizeFeatures(msdata, FUN=list(mean, sum))

	expect_equal(iData(tmp1)[,1], rowMeans(iData(msdata)))

	expect_equal(iData(tmp2)[,1], rowMeans(iData(msdata)))

	expect_equal(iData(tmp2)[,2], rowSums(iData(msdata)))

	tmp3 <- summarizeFeatures(msdata, FUN=mean, groups=run(msdata))

	tmp4 <- summarizeFeatures(msdata, FUN=list(mean, sum), groups=run(msdata))

	a1 <- t(apply(iData(msdata), 1, function(s) {
		tapply(s, run(msdata), mean)
	}))

	a2 <- t(apply(iData(msdata), 1, function(s) {
		tapply(s, run(msdata), sum)
	}))

	expect_equivalent(iData(tmp3), a1)

	expect_equivalent(iData(tmp4)[,1:2], a1)

	expect_equivalent(iData(tmp4)[,3:4], a2)

})

test_that("summarize (streamstat)", {

	tmp1 <- summarizeFeatures(msdata, "mean")

	tmp2 <- summarizeFeatures(msdata, c("mean", "sum"))

	expect_equal(iData(tmp1)[,1], rowMeans(iData(msdata)))

	expect_equal(iData(tmp2)[,1], rowMeans(iData(msdata)))

	expect_equal(iData(tmp2)[,2], rowSums(iData(msdata)))

	tmp3 <- summarizeFeatures(msdata, "mean", groups=run(msdata))

	tmp4 <- summarizeFeatures(msdata, c("mean", "sum"), groups=run(msdata))

	a1 <- t(apply(iData(msdata), 1, function(s) {
		tapply(s, run(msdata), mean)
	}))

	a2 <- t(apply(iData(msdata), 1, function(s) {
		tapply(s, run(msdata), sum)
	}))

	expect_equivalent(iData(tmp3), a1)

	expect_equivalent(iData(tmp4)[,1:2], a1)

	expect_equivalent(iData(tmp4)[,3:4], a2)

})
