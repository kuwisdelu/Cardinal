require(testthat)
require(Cardinal)

context("new processing")

test_that("summarize expr", {

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

	tmp1 <- summarize(msdata, mean(.))

	expect_equal(ncol(tmp1), 1L)

	tmp2 <- summarize(msdata, mean(.), sum(.))

	expect_equal(ncol(tmp2), 2L)

	tmp3 <- summarize(msdata, mean(.), sum(.), .group_by=cond)

	expect_equal(ncol(tmp3), 4L)

	tmp4 <- summarize(msdata, mean(.), sum(.), .group_by="cond")

	expect_equal(ncol(tmp4), 4L)

	tmp5 <- summarize(msdata, mean(.), sum(.), .group_by=c("run", "cond"))

	expect_equal(ncol(tmp5), 8L)

	tmp6 <- summarize(msdata, mean(.), sum(.), .group_by=~run * cond)

	expect_equal(ncol(tmp6), 8L)

	tmp7 <- summarize(msdata, mean(.), sum(.), .group_by=run(msdata))

	expect_equal(ncol(tmp7), 4L)

	tmp8 <- summarize(msdata, mean, .group_by=run(msdata))

	expect_equal(ncol(tmp8), 2L)

	tmp9 <- summarize(msdata, mean, sum, .group_by=run(msdata))

	expect_equal(ncol(tmp9), 4L)

})

test_that("summarize stat", {

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

	tmp1 <- summarize(msdata, .stat="mean")

	expect_equal(ncol(tmp1), 1L)

	tmp2 <- summarize(msdata, .stat=c("mean", "sum"), .group_by=cond)

	expect_equal(ncol(tmp2), 4L)

	tmp3 <- summarize(msdata, .stat=c("mean", "sum"), .group_by=~cond * run)

	expect_equal(ncol(tmp3), 8L)

	tmp4 <- summarize(msdata, .group_by=run(msdata))

	expect_equal(ncol(tmp4), 12L)

	msdata2 <- msdata

	spectra(msdata2) <- matter::as.sparse(spectra(msdata2))

	tmp5 <- summarize(msdata2, .stat="mean", .group_by=run)

	expect_equal(ncol(tmp5), 2L)

	tmp6 <- summarize(msdata2, .stat=c("mean", "var"), .group_by=run)

	expect_equal(ncol(tmp6), 4L)

	tmp7 <- summarize(msdata2, .stat=c("mean", "var"), .group_by=~run * cond)

	expect_equal(ncol(tmp7), 8L)

	tmp8 <- summarize(msdata2, .stat=c("min", "max", "mean", "sd"))

	expect_equal(ncol(tmp8), 4L)

	tmp9 <- summarize(msdata2, .group_by=run(msdata2))

	expect_equivalent(tmp4, tmp9)

})
