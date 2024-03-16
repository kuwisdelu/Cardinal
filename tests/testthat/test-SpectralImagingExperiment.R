require(testthat)
require(Cardinal)

context("SpectralImagingExperiment")

test_that("SpectralImagingExperiment accessors", {

	se <- SpectralImagingExperiment()

	expect_true(validObject(se))

	se <- SpectralImagingExperiment(matrix(nrow=0, ncol=0))

	expect_true(validObject(se))

	set.seed(1)
	nr <- 20
	nc <- 10
	a <- matrix(rlnorm(nr * nc), nrow=nr, ncol=nc)
	s <- SpectraArrays(a)
	fdata <- XDataFrame(index=seq_len(nr),
		wavelength=seq(900, 1000, length.out=nr), keys=list(index="index"))
	fdata2 <- XDataFrame(index=nr+seq_len(nr),
		wavelength=seq(1001, 1100, length.out=nr), keys=list(index="index"))
	pdata <- PositionDataFrame(
		coord=expand.grid(x=1:5, y=1:2),
		diagnosis=rep(c("yes", "no"), each=5))
	pdata2 <- PositionDataFrame(
		coord=expand.grid(x=1:5, y=3:4),
		diagnosis=rep(c("yes", "no"), each=5))
	se <- SpectralImagingExperiment(s, featureData=fdata, pixelData=pdata)

	expect_true(validObject(se))
	expect_length(se, nc)
	expect_equal(dim(se), c(nr, nc))
	
	expect_equal(spectraData(se), s)
	expect_equal(pixelData(se), pdata)
	expect_equal(featureData(se), fdata)
	
	expect_equal(spectra(se), s[[1L]])
	expect_equal(pData(se), pdata)
	expect_equal(fData(se), fdata)

	expect_equal(coord(se), coord(pdata))
	expect_equal(run(se), run(pdata))
	expect_equal(nrun(se), nrun(pdata))
	expect_equal(se$diagnosis, pdata$diagnosis)

	w1 <- 920
	w2 <- 975

	expect_setequal(features(se, 1:10), 1:10)
	expect_setequal(features(se, wavelength > 920), 5:20)
	expect_setequal(features(se, wavelength < 975), 1:15)
	expect_setequal(features(se, wavelength > w1), 5:20)
	expect_setequal(features(se, wavelength < w2), 1:15)
	expect_setequal(features(se, 1:10, wavelength > 920), 5:10)
	expect_setequal(features(se, 1:10, wavelength < 975), 1:10)
	expect_setequal(features(se, 1:10, wavelength > w1), 5:10)
	expect_setequal(features(se, 1:10, wavelength < w2), 1:10)

	expect_equal(subsetPixels(se, 1:5), se[,1:5])
	expect_equal(subsetPixels(se, diagnosis == "yes"), se[,se$diagnosis == "yes"])
	expect_equal(subsetFeatures(se, 1:10), se[1:10,])
	expect_equal(subsetFeatures(se, wavelength > w1), se[5:20,])
	expect_equal(subsetFeatures(se, wavelength < w2), se[1:15,])
	expect_equal(subset(se, wavelength > w1, 1:5), se[5:20,1:5])
	expect_equal(subset(se, wavelength < w2, 1:5), se[1:15,1:5])

	pixelData(se) <- pdata2
	featureData(se) <- fdata2

	expect_true(validObject(se))
	expect_equal(pixelData(se), pdata2)
	expect_equal(featureData(se), fdata2)

	coord(se) <- coord(pdata)
	run(se) <- run(pdata)

	expect_equal(coord(se), coord(pdata))
	expect_equal(run(se), run(pdata))

	se2 <- se[1:10,1:5]
	se3 <- se[1:10,]
	se4 <- se[,1:5]

	expect_true(validObject(se2))
	expect_equal(dim(se2), c(10,5))
	expect_equal(spectra(se2), spectra(se)[1:10,1:5])
	expect_equal(pixelData(se2), pixelData(se)[1:5,])
	expect_equal(featureData(se2), featureData(se)[1:10,])
	
	expect_true(validObject(se3))
	expect_equal(dim(se3), c(10,10))
	expect_equal(spectra(se3), spectra(se)[1:10,])
	expect_equal(featureData(se2), featureData(se)[1:10,])
	
	expect_true(validObject(se4))
	expect_equal(dim(se4), c(20,5))
	expect_equal(spectra(se4), spectra(se)[,1:5])
	expect_equal(pixelData(se2), pixelData(se)[1:5,])

})

test_that("SpectralImagingExperiment rbind/cbind", {

	set.seed(1)
	nr <- 20
	nc <- 10
	a <- matrix(rlnorm(nr * nc), nrow=nr, ncol=nc)
	s <- SpectraArrays(a)
	fdata <- XDataFrame(index=seq_len(nr),
		wavelength=seq(900, 1000, length.out=nr), keys=list(index="index"))
	fdata2 <- XDataFrame(index=nr+seq_len(nr),
		wavelength=seq(1001, 1100, length.out=nr), keys=list(index="index"))
	pdata <- PositionDataFrame(
		coord=expand.grid(x=1:5, y=1:2),
		diagnosis=rep(c("yes", "no"), each=5))
	pdata2 <- PositionDataFrame(
		coord=expand.grid(x=1:5, y=3:4),
		diagnosis=rep(c("yes", "no"), each=5))
	se <- SpectralImagingExperiment(s, featureData=fdata, pixelData=pdata)
	se02 <- SpectralImagingExperiment(s, featureData=fdata, pixelData=pdata2)
	se20 <- SpectralImagingExperiment(s, featureData=fdata2, pixelData=pdata)

	r00 <- rbind(se, se)
	expect_error(r02 <- rbind(se, se02))
	r20 <- rbind(se, se20)

	expect_equal(spectra(r00), rbind(spectra(se), spectra(se)))
	expect_equal(fData(r00), rbind(fData(se), fData(se)))
	expect_equal(pData(r00), cbind(pData(se), pData(se)))
	expect_equal(spectra(r20), rbind(spectra(se), spectra(se20)))
	expect_equal(fData(r20), rbind(fData(se), fData(se20)))
	expect_equal(pData(r20), cbind(pData(se), pData(se20)))

	c00 <- cbind(se, se)
	c02 <- cbind(se, se02)
	expect_error(c20 <- cbind(se, se20))

	expect_equal(spectra(c00), cbind(spectra(se), spectra(se)))
	expect_equal(fData(c00), cbind(fData(se), fData(se)))
	expect_equal(pData(c00), rbind(pData(se), pData(se)))
	expect_equal(spectra(c02), cbind(spectra(se), spectra(se02)))
	expect_equal(fData(c02), cbind(fData(se), fData(se02)))
	expect_equal(pData(c02), rbind(pData(se), pData(se02)))

})
