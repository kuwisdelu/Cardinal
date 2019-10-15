require(testthat)
require(Cardinal)

context("summarize")

options(Cardinal.progress=FALSE, Cardinal.verbose=FALSE)

register(SerialParam())

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

test_that("dplyr manip", {

	tmp1 <- filter(msdata, mz > 101)

	tmp2 <- msdata[mz(msdata) > 101,]

	expect_equal(tmp1, tmp2)

	i1 <- 1:10

	tmp3 <- filter(msdata, i1)

	tmp4 <- msdata[i1,]

	expect_equal(tmp3, tmp4)

	tmp5 <- select(msdata, x > 5)

	tmp6 <- msdata[,coord(msdata)$x > 5]

	expect_equal(tmp5, tmp6)

	i2 <- 1:10

	tmp7 <- select(msdata, i2)

	tmp8 <- msdata[,i2]

	expect_equal(tmp7, tmp8)

	tmp9 <- mutate(msdata,
		test1=c("a", "b"),
		test2=paste0(test1, "1"))

	msdata$test1 <- c("a", "b")
	msdata$test2 <- paste0(msdata$test1, "1")

	expect_equal(tmp9, msdata)

})

test_that("summarize expr", {

	tmp1 <- summarize(msdata, mean(.))

	expect_equal(iData(tmp1)[,1], rowMeans(iData(msdata)))

	tmp2 <- summarize(msdata, mean(.), sum(.))

	expect_equal(iData(tmp2)[,1], rowMeans(iData(msdata)))

	expect_equal(iData(tmp2)[,2], rowSums(iData(msdata)))

	tmp3 <- summarize(msdata, mean(.), sum(.), .groups=run(msdata))

	a1 <- t(apply(iData(msdata), 1, function(s) {
		tapply(s, run(msdata), mean)
	}))

	a2 <- t(apply(iData(msdata), 1, function(s) {
		tapply(s, run(msdata), sum)
	}))

	expect_equivalent(iData(tmp3)[,1:2], a1)

	expect_equivalent(iData(tmp3)[,3:4], a2)

})

test_that("summarize stat", {

	tmp1 <- summarize(msdata, .stat="mean")

	expect_equal(iData(tmp1)[,1], rowMeans(iData(msdata)))

	tmp2 <- summarize(msdata, .stat=c("mean", "sum"))

	expect_equal(iData(tmp2)[,1], rowMeans(iData(msdata)))

	expect_equal(iData(tmp2)[,2], rowSums(iData(msdata)))

	tmp3 <- summarize(msdata, .stat=c("mean", "sum"), .groups=run(msdata))

	a1 <- t(apply(iData(msdata), 1, function(s) {
		tapply(s, run(msdata), mean)
	}))

	a2 <- t(apply(iData(msdata), 1, function(s) {
		tapply(s, run(msdata), sum)
	}))

	expect_equivalent(iData(tmp3)[,1:2], a1)

	expect_equivalent(iData(tmp3)[,3:4], a2)

	msdata2 <- msdata

	tmp4 <- summarize(msdata2, .stat="mean")

	expect_equal(iData(tmp4)[,1], rowMeans(iData(msdata2)))

	tmp5 <- summarize(msdata2, .stat=c("mean", "sum"))

	expect_equal(iData(tmp5)[,1], rowMeans(iData(msdata2)))

	expect_equal(iData(tmp5)[,2], rowSums(iData(msdata2)))

	tmp6 <- summarize(msdata2, .stat=c("mean", "sum"), .groups=run(msdata2))

	a3 <- t(apply(iData(msdata2), 1, function(s) {
		tapply(s, run(msdata2), mean)
	}))

	a4 <- t(apply(iData(msdata2), 1, function(s) {
		tapply(s, run(msdata2), sum)
	}))

	expect_equivalent(iData(tmp6)[,1:2], a3)

	expect_equivalent(iData(tmp6)[,3:4], a4)

})
