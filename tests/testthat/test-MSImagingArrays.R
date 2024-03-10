
context("MSImagingArrays")

test_that("MSImagingArrays accessors", {

	msa <- MSImagingArrays()

	expect_true(validObject(msa))

	msa <- MSImagingArrays(numeric(0))

	expect_true(validObject(msa))

	set.seed(1)
	n <- 10
	i <- rep(list(501:510), n)
	a <- replicate(n, rlnorm(n), simplify=FALSE)
	s <- SpectraArrays(list(mz=i, intensity=a))
	pdata <- PositionDataFrame(
		coord=expand.grid(x=1:5, y=1:2),
		diagnosis=rep(c("yes", "no"), each=5))
	pdata2 <- PositionDataFrame(
		coord=expand.grid(x=1:5, y=3:4),
		diagnosis=rep(c("yes", "no"), each=5))
	expdata <- ImzMeta(spectrumType="MS1 spectrum",
		spectrumRepresentation="profile",
		contactName="Kylie Ariel Bemis")
	msa <- MSImagingArrays(s, pixelData=pdata,
		experimentData=NULL, centroided=FALSE)

	expect_true(validObject(msa))
	expect_length(msa, n)
	expect_null(dim(msa))
	
	expect_equal(spectraData(msa), s)
	expect_equal(pixelData(msa), pdata)
	expect_equal(experimentData(msa), NULL)
	expect_false(centroided(msa))
	
	expect_equal(spectra(msa, 1L), s[[1L]])
	expect_equal(spectra(msa, 2L), s[[2L]])
	expect_equal(pData(msa), pdata)

	expect_equal(mz(msa), s[["mz"]])
	expect_equal(mz(msa, 1L), s[["mz"]][[1L]])
	expect_equal(mz(msa, 10L), s[["mz"]][[10L]])
	expect_equal(intensity(msa), s[["intensity"]])
	expect_equal(intensity(msa, 1L), s[["intensity"]][[1L]])
	expect_equal(intensity(msa, 10L), s[["intensity"]][[10L]])
	expect_equal(coord(msa), coord(pdata))
	expect_equal(run(msa), run(pdata))
	expect_equal(nrun(msa), nrun(pdata))
	expect_equal(msa$diagnosis, pdata$diagnosis)

	experimentData(msa) <- expdata

	expect_true(validObject(msa))
	expect_equal(experimentData(msa), expdata)

	centroided(msa) <- NA

	expect_equal(centroided(msa), NA)
	expect_false(isCentroided(msa))

	msa2 <- msa[2:9]

	expect_true(validObject(msa2))
	expect_equal(length(msa2), 8L)
	expect_equal(mz(msa2), mz(msa)[2:9])
	expect_equal(intensity(msa2), intensity(msa)[2:9])
	expect_equal(pixelData(msa2), pixelData(msa)[2:9,])

})

test_that("MSImagingArrays combine", {

	set.seed(1)
	n <- 10
	i <- rep(list(501:510), n)
	a <- replicate(n, rlnorm(n), simplify=FALSE)
	s <- SpectraArrays(list(mz=i, intensity=a))
	pdata <- PositionDataFrame(
		coord=expand.grid(x=1:5, y=1:2),
		diagnosis=rep(c("yes", "no"), each=5))
	pdata2 <- PositionDataFrame(
		coord=expand.grid(x=1:5, y=3:4),
		diagnosis=rep(c("yes", "no"), each=5))
	expdata <- ImzMeta(spectrumType="MS1 spectrum",
		spectrumRepresentation="profile",
		contactName="Kylie Ariel Bemis")
	msa <- MSImagingArrays(s, pixelData=pdata,
		experimentData=NULL, centroided=FALSE)
	msa2 <- MSImagingArrays(s, pixelData=pdata2)

	msa3 <- c(msa, msa2)

	expect_equal(mz(msa3), c(mz(msa), mz(msa2)))
	expect_equal(intensity(msa3), c(intensity(msa), intensity(msa2)))
	expect_equal(pData(msa3), rbind(pData(msa), pData(msa2)))

})
