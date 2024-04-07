require(testthat)
require(Cardinal)

context("MSImagingExperiment")

test_that("MSImagingExperiment accessors", {

	mse <- MSImagingExperiment()

	expect_true(validObject(mse))

	mse <- MSImagingExperiment(matrix(nrow=0, ncol=0))

	expect_true(validObject(mse))

	set.seed(1)
	nr <- 20
	nc <- 10
	a <- matrix(rlnorm(nr * nc), nrow=nr, ncol=nc)
	s <- SpectraArrays(a)
	fdata <- MassDataFrame(mz=seq(500, 550, length.out=nr),
		label=rep.int("light", 10))
	fdata2 <- MassDataFrame(mz=seq(551, 600, length.out=nr),
		label=rep.int("heavy", 10))
	pdata <- PositionDataFrame(
		coord=expand.grid(x=1:5, y=1:2),
		diagnosis=rep(c("yes", "no"), each=5))
	pdata2 <- PositionDataFrame(
		coord=expand.grid(x=1:5, y=3:4),
		diagnosis=rep(c("yes", "no"), each=5))
	expdata <- CardinalIO::ImzMeta(spectrumType="MS1 spectrum",
		spectrumRepresentation="profile",
		contactName="Kylie Ariel Bemis")
	mse <- MSImagingExperiment(s, featureData=fdata, pixelData=pdata,
		experimentData=NULL, centroided=FALSE)

	expect_true(validObject(mse))
	expect_length(mse, nc)
	expect_equal(dim(mse), c(nr, nc))
	
	expect_equal(spectraData(mse), s)
	expect_equal(pixelData(mse), pdata)
	expect_equal(featureData(mse), fdata)
	expect_equal(experimentData(mse), NULL)
	expect_false(centroided(mse))
	
	expect_equal(spectra(mse), s[[1L]])
	expect_equal(pData(mse), pdata)
	expect_equal(fData(mse), fdata)

	expect_equal(mz(mse), mz(fdata))
	expect_equal(coord(mse), coord(pdata))
	expect_equal(run(mse), run(pdata))
	expect_equal(mse$diagnosis, pdata$diagnosis)

	m1 <- 505.3
	m2 <- 544.7

	expect_setequal(features(mse, label=="light"), 1:20)
	expect_setequal(features(mse, mz=505), 3)
	expect_setequal(features(mse, mz=545), 18)
	expect_setequal(features(mse, mz=m1), 3)
	expect_setequal(features(mse, mz=m2), 18)
	expect_setequal(features(mse, mz=c(m1, m2)), c(3,18))

	mz(mse) <- mz(fdata2)
	experimentData(mse) <- expdata

	expect_true(validObject(mse))
	expect_equal(mz(mse), mz(fdata2))
	expect_equal(experimentData(mse), expdata)

	centroided(mse) <- NA

	expect_equal(centroided(mse), NA)
	expect_false(isCentroided(mse))

	mse2 <- mse[1:10,1:5]
	mse3 <- mse[1:10,]
	mse4 <- mse[,1:5]

	expect_true(validObject(mse2))
	expect_equal(dim(mse2), c(10,5))
	expect_equal(spectra(mse2), spectra(mse)[1:10,1:5])
	expect_equal(pixelData(mse2), pixelData(mse)[1:5,])
	expect_equal(featureData(mse2), featureData(mse)[1:10,])
	
	expect_true(validObject(mse3))
	expect_equal(dim(mse3), c(10,10))
	expect_equal(spectra(mse3), spectra(mse)[1:10,])
	expect_equal(featureData(mse2), featureData(mse)[1:10,])
	
	expect_true(validObject(mse4))
	expect_equal(dim(mse4), c(20,5))
	expect_equal(spectra(mse4), spectra(mse)[,1:5])
	expect_equal(pixelData(mse2), pixelData(mse)[1:5,])

})

test_that("MSImagingExperiment rbind/cbind", {

	set.seed(1)
	nr <- 20
	nc <- 10
	a <- matrix(rlnorm(nr * nc), nrow=nr, ncol=nc)
	s <- SpectraArrays(a)
	fdata <- MassDataFrame(mz=seq(500, 550, length.out=nr),
		label=rep.int("light", 10))
	fdata2 <- MassDataFrame(mz=seq(551, 600, length.out=nr),
		label=rep.int("heavy", 10))
	pdata <- PositionDataFrame(
		coord=expand.grid(x=1:5, y=1:2),
		diagnosis=rep(c("yes", "no"), each=5))
	pdata2 <- PositionDataFrame(
		coord=expand.grid(x=1:5, y=3:4),
		diagnosis=rep(c("yes", "no"), each=5))
	expdata <- CardinalIO::ImzMeta(spectrumType="MS1 spectrum",
		spectrumRepresentation="profile",
		contactName="Kylie Ariel Bemis")
	mse <- MSImagingExperiment(s, featureData=fdata, pixelData=pdata,
		experimentData=NULL, centroided=FALSE)
	mse02 <- MSImagingExperiment(s, featureData=fdata, pixelData=pdata2)
	mse20 <- MSImagingExperiment(s, featureData=fdata2, pixelData=pdata)

	r20 <- rbind(mse, mse20)

	expect_equal(spectra(r20), rbind(spectra(mse), spectra(mse20)))
	expect_equal(fData(r20), rbind(fData(mse), fData(mse20)))
	expect_equal(pData(r20), cbind(pData(mse), pData(mse20)))

	c02 <- cbind(mse, mse02)

	expect_equal(spectra(c02), cbind(spectra(mse), spectra(mse02)))
	expect_equal(fData(c02), cbind(fData(mse), fData(mse02)))
	expect_equal(pData(c02), rbind(pData(mse), pData(mse02)))

})
