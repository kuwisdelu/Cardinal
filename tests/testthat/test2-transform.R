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

test_that("summarize expr", {

	tmp1 <- summarize(msdata, mean(.))

	expect_equal(ncol(tmp1), 1L)

	tmp2 <- summarize(msdata, mean(.), sum(.))

	expect_equal(ncol(tmp2), 2L)

	tmp3 <- summarize(msdata, mean(.), sum(.), .group_by=msdata$cond)

	expect_equal(ncol(tmp3), 4L)

	tmp4 <- summarize(msdata, mean(.), sum(.), .group_by=run(msdata))

	expect_equal(ncol(tmp4), 4L)

	tmp5 <- summarize(msdata, mean, .group_by=run(msdata))

	expect_equal(ncol(tmp5), 2L)

	tmp6 <- summarize(msdata, mean, sum, .group_by=run(msdata))

	expect_equal(ncol(tmp6), 4L)

})

test_that("summarize stat", {

	tmp1 <- summarize(msdata, .stat="mean")

	expect_equal(ncol(tmp1), 1L)

	tmp2 <- summarize(msdata, .stat=c("mean", "sum"), .group_by=msdata$cond)

	expect_equal(ncol(tmp2), 4L)

	tmp3 <- summarize(msdata, .stat=c("sd", "mean"), .group_by=run(msdata))

	expect_equal(ncol(tmp3), 4L)

	tmp4 <- summarize(msdata, .group_by=run(msdata))

	expect_equal(ncol(tmp4), 12L)

	msdata2 <- msdata

	spectra(msdata2) <- matter::as.sparse(spectra(msdata2))

	tmp5 <- summarize(msdata2, .stat="mean")

	expect_equal(ncol(tmp5), 1L)

	tmp6 <- summarize(msdata2, .stat="mean", .group_by=run(msdata2))

	expect_equal(ncol(tmp6), 2L)

	tmp7 <- summarize(msdata2, .stat=c("mean", "sd"), .group_by=msdata$cond)

	expect_equal(ncol(tmp7), 4L)

	tmp8 <- summarize(msdata2, .stat=c("mean", "var", "min", "max"))

	expect_equal(ncol(tmp8), 4L)

	tmp9 <- summarize(msdata2, .group_by=run(msdata2))

	expect_equivalent(tmp4, tmp9)

})
