require(testthat)
require(Cardinal)

context("stats")

test_that("PCA", {

	set.seed(1)
	s <- simulateImage(preset=1, dim=c(10L, 10L))
	pc <- PCA(s, ncomp=2)
	pred <- predict(pc, newdata=s)
	
	x <- t(spectra(s))
	pc2 <- prcomp(x)
	pred2 <- predict(pc2, newdata=x)

	expect_equal(pc$sdev, pc2$sdev[1:2])
	expect_equal(abs(pc$x), abs(pc2$x[,1:2]))
	expect_equal(abs(pc$rotation), abs(pc2$rotation[,1:2]))
	expect_equal(abs(pred), abs(pred2[,1:2]))

})

test_that("NMF", {

	set.seed(1)
	s <- simulateImage(preset=1, dim=c(10L, 10L))
	mf <- NMF(s, ncomp=2, method="als")
	pred <- predict(mf, newdata=s)
	
	mf2 <- NMF(s, ncomp=2, method="mult")
	pred2 <- predict(mf2, newdata=s)

	expect_equivalent(mf$x, pred, tolerance=0.05)
	expect_equivalent(mf2$x, pred2, tolerance=0.05)

})

test_that("PLS", {

	set.seed(1)
	s <- simulateImage(preset=1, dim=c(10L, 10L), nrun=2L)
	s$class <- makeFactor(yes=s$circle, no=!s$circle)
	pl <- PLS(s, s$class, ncomp=2, method="nipals")
	pred <- predict(pl, type="class")
	pred01 <- predict(pl, newdata=s, ncomp=1)
	pred02 <- predict(pl, newdata=s, ncomp=2)
	pred012 <- predict(pl, newdata=s, ncomp=1:2)
	topf <- topFeatures(pl)

	expect_is(pred, "factor")
	expect_equal(levels(pred), c("yes", "no"))
	expect_equal(fitted(pl, "class"), pred)
	expect_equivalent(pred01, pred012[,,1L])
	expect_equivalent(pred02, pred012[,,2L])
	expect_equivalent(fitted(pl), pred02)
	expect_is(topf, "DataFrame")
	expect_false(is.unsorted(rev(topf$vip)))

	pl2 <- PLS(s, s$class, ncomp=2, method="simpls")
	pred2 <- predict(pl2, newdata=s)
	pl3 <- PLS(s, s$class, ncomp=2, method="kernel1")
	pred3 <- predict(pl3, newdata=s)
	pl4 <- PLS(s, s$class, ncomp=2, method="kernel2")
	pred4 <- predict(pl4, newdata=s)

	expect_equivalent(fitted(pl2), pred2)
	expect_equivalent(fitted(pl3), pred3)
	expect_equivalent(fitted(pl4), pred4)

})

test_that("OPLS", {

	set.seed(1)
	s <- simulateImage(preset=1, dim=c(10L, 10L))
	s$class <- makeFactor(yes=s$circle, no=!s$circle)
	op <- OPLS(s, s$class, ncomp=1:2)
	topf <- topFeatures(op)

	pred <- predict(op, type="class")
	pred1 <- predict(op, newdata=s, ncomp=1)
	pred2 <- predict(op, newdata=s, ncomp=2)
	pred12 <- predict(op, newdata=s, ncomp=1:2)

	expect_equal(pred, s$class)
	expect_equal(pred, fitted(op, "class"))
	expect_equal(pred1, op$regressions[[1L]]$fitted.values)
	expect_equal(pred2, op$regressions[[2L]]$fitted.values)
	expect_is(topf, "DataFrame")
	expect_false(is.unsorted(rev(topf$vip)))

})

test_that("spatialFastmap", {

	set.seed(1)
	s <- simulateImage(preset=1, dim=c(10L, 10L))
	fm <- spatialFastmap(s, ncomp=2, weights="gaussian")
	pred <- predict(fm, newdata=s)
	
	fm2 <- spatialFastmap(s, ncomp=2, weights="adaptive")
	pred2 <- predict(fm2, newdata=s)

	expect_equal(fm$x, pred)
	expect_equal(fm2$x, pred2)

})

test_that("spatialKMeans", {

	set.seed(1)
	s <- simulateImage(preset=2, dim=c(10L, 10L),
		representation="centroid")
	km <- spatialKMeans(s, k=2:4, weights="gaussian")
	km2 <- spatialKMeans(s, k=2:4, weights="adaptive")
	topf <- topFeatures(km)

	expect_true(validObject(km))
	expect_true(validObject(km2))
	expect_length(km, 3L)
	expect_length(km2, 3L)
	expect_is(km, "ResultsList")
	expect_is(km2, "ResultsList")
	expect_is(km[[1L]], "SpatialKMeans")
	expect_is(km2[[1L]], "SpatialKMeans")
	expect_setequal(km[[1L]]$cluster, 1:4)
	expect_setequal(km[[2L]]$cluster, 1:3)
	expect_setequal(km[[3L]]$cluster, 1:2)
	expect_equal(ncol(km[[1L]]$correlation), 4L)
	expect_equal(ncol(km[[2L]]$correlation), 3L)
	expect_equal(ncol(km[[3L]]$correlation), 2L)
	expect_is(topf[[1L]], "DataFrame")
	expect_false(is.unsorted(rev(topf[[1L]]$correlation)))

})

test_that("spatialShrunkenCentroids (classification)", {

	set.seed(1)
	s <- simulateImage(preset=2, dim=c(10L, 10L), nrun=2L,
		representation="centroid")
	s$class <- makeFactor(circle=s$circle, square=s$square,
		bg=!s$circle & !s$square)

	ssc <- spatialShrunkenCentroids(s, s$class, s=0:3, weights="gaussian")
	ssc2 <- spatialShrunkenCentroids(s, s$class, s=0:3, weights="adaptive")
	pred <- predict(ssc[[1L]], newdata=s, type="class")
	pred2 <- predict(ssc2[[1L]], newdata=s, type="class")
	pred12 <- predict(ssc2[1:2], newdata=s, type="class")
	topf <- topFeatures(ssc)

	expect_true(validObject(ssc))
	expect_true(validObject(ssc2))
	expect_length(ssc, 4L)
	expect_length(ssc2, 4L)
	expect_is(ssc, "ResultsList")
	expect_is(ssc2, "ResultsList")
	expect_is(ssc[[1L]], "SpatialShrunkenCentroids")
	expect_is(ssc2[[1L]], "SpatialShrunkenCentroids")
	expect_equal(fitted(ssc[[1L]], "class"), pred)
	expect_equal(fitted(ssc2[[1L]], "class"), pred2)
	expect_is(topf[[1L]], "DataFrame")
	expect_false(is.unsorted(rev(topf[[1L]]$statistic)))

	s$one <- rep.int(factor("one"), length(s))
	ssc3 <- spatialShrunkenCentroids(s, s$one, s=3)

	expect_equal(ssc3$class, as.factor(s$one))
	expect_true(all(ssc3$statistic == 0))
	expect_equivalent(ssc3$centers, rowMeans(spectra(s)))

})

test_that("spatialShrunkenCentroids (clustering)", {

	set.seed(1)
	s <- simulateImage(preset=2, dim=c(10L, 10L),
		representation="centroid")
	s$class <- makeFactor(circle=s$circle, square=s$square,
		bg=!s$circle & !s$square)

	set.seed(2)
	ssc <- spatialShrunkenCentroids(s, k=2:3, s=0:3, weights="gaussian")
	ssc2 <- spatialShrunkenCentroids(s, k=2:3, s=0:3, weights="adaptive")
	pred <- predict(ssc[[1L]], newdata=s, type="class")
	pred2 <- predict(ssc2[[1L]], newdata=s, type="class")
	pred3 <- predict(ssc2, newdata=s, type="class")
	topf <- topFeatures(ssc)

	expect_true(validObject(ssc))
	expect_true(validObject(ssc2))
	expect_length(ssc, 8L)
	expect_length(ssc2, 8L)
	expect_is(ssc, "ResultsList")
	expect_is(ssc2, "ResultsList")
	expect_is(ssc[[1L]], "SpatialShrunkenCentroids")
	expect_is(ssc2[[1L]], "SpatialShrunkenCentroids")
	expect_equal(fitted(ssc[[1L]], "class"), pred)
	expect_equal(fitted(ssc2[[1L]], "class"), pred2)
	expect_equal(fitted(ssc2[[1L]], "class"), pred3[[1L]])
	expect_equal(fitted(ssc2[[2L]], "class"), pred3[[2L]])
	expect_is(topf[[1L]], "DataFrame")
	expect_false(is.unsorted(rev(topf[[1L]]$statistic)))

	set.seed(3)
	ssc3 <- spatialShrunkenCentroids(s, k=1, s=0)

	expect_setequal(ssc3$class, factor(1L))
	expect_true(all(ssc3$statistic == 0))
	expect_equivalent(ssc3$centers, rowMeans(spectra(s)))

	set.seed(4)
	ssc4 <- spatialShrunkenCentroids(s, k=4, s=12)

	expect_lte(nlevels(ssc3$class), 4)

	set.seed(5)
	coord <- expand.grid(x=1:9, y=1:9)
	n <- nrow(coord)
	p <- 10
	x <- matrix(rnorm(n * p), nrow=p, ncol=n)
	s2 <- SpectralImagingExperiment(x,
		pixelData=PositionDataFrame(coord))
	ssc5 <- spatialShrunkenCentroids(s2, k=5, s=0)

	expect_lte(nlevels(ssc5$class), 5)

	set.seed(6)
	expect_warning(spatialShrunkenCentroids(s2, k=6, s=12))

})

test_that("spatialDGMM", {

	set.seed(1)
	s <- simulateImage(preset=2, dim=c(10L, 10L), nrun=2L,
		representation="centroid")

	set.seed(2)
	gm <- spatialDGMM(s, k=4, weights="gaussian")
	gm2 <- spatialDGMM(s, k=4, weights="adaptive")

	expect_true(validObject(gm))
	expect_true(validObject(gm2))
	expect_is(gm, "SpatialDGMM")
	expect_is(gm2, "SpatialDGMM")
	expect_length(gm$class, nrow(s))
	expect_length(gm2$class, nrow(s))
	expect_is(gm$class[[1L]], "drle_fct")
	expect_is(gm2$class[[1L]], "drle_fct")
	expect_setequal(as.factor(gm$class[[1L]]), factor(1:4))
	expect_setequal(as.factor(gm2$class[[1L]]), factor(1:4))

	set.seed(3)
	i <- seq_len(15)
	gm3 <- spatialDGMM(s, i=i, r=2, k=3:5, compress=FALSE)

	expect_is(gm3, "ResultsList")
	expect_length(gm3, 3L)
	expect_length(gm3[[1L]]$class, length(i))
	expect_setequal(as.factor(gm3[[1L]]$class[[1L]]), factor(1:5))
	expect_setequal(as.factor(gm3[[2L]]$class[[1L]]), factor(1:4))
	expect_setequal(as.factor(gm3[[3L]]$class[[1L]]), factor(1:3))

})

test_that("meansTest", {

	set.seed(1)
	s <- simulateImage(preset=4, dim=c(10L, 10L), nrun=3,
		representation="centroid")
	s$truecondition <- ifelse(s$circleA | s$circleB, s$condition, NA)
	s$truecondition <- factor(s$truecondition)
	levels(s$truecondition) <- levels(s$condition)
	s$sample <- replace(run(s), is.na(s$truecondition), NA)
	featureNames(s) <- paste0("V", seq_len(nrow(s)))

	mt <- meansTest(s, fixed=~condition)
	mt2 <- meansTest(s, fixed=~truecondition)
	mt3 <- meansTest(s, fixed=~condition, samples=s$sample)
	topf <- topFeatures(mt)

	expect_true(validObject(mt))
	expect_true(validObject(mt2))
	expect_true(validObject(mt3))
	expect_true(all(mcols(mt)$statistic > 0))
	expect_true(all(mcols(mt2)$statistic > 0))
	expect_true(all(mcols(mt3)$statistic > 0))
	expect_true(all(mcols(mt)$pvalue > 0))
	expect_true(all(mcols(mt2)$pvalue > 0))
	expect_true(all(mcols(mt3)$pvalue > 0))
	expect_is(topf, "DataFrame")
	expect_false(is.unsorted(rev(topf$statistic)))

	set.seed(2)
	s2 <- simulateImage(preset=4, dim=c(10L, 10L), nrun=1,
		representation="centroid")
	featureNames(s2) <- paste0("Feature", seq_len(nrow(s2)))
	
	mt4 <- meansTest(s2, fixed=~condition)

	expect_true(validObject(mt4))
	expect_true(all(is.infinite(mcols(mt4)$statistic)))
	expect_true(all(mcols(mt4)$pvalue <= 0))

	set.seed(3)
	gm <- spatialDGMM(s, r=1, k=2)
	gm2 <- spatialDGMM(s2, r=1, k=2)

	mt5 <- meansTest(gm, ~condition)
	mt6 <- meansTest(gm, ~truecondition)
	mt7 <- meansTest(gm2, ~condition)
	st <- segmentationTest(s, ~condition)

	expect_equal(mcols(mt5)$statistic, mcols(mt6)$statistic)
	expect_equal(mcols(mt5)$pvalue, mcols(mt6)$pvalue)
	expect_true(all(is.infinite(mcols(mt7)$statistic)))
	expect_true(all(mcols(mt7)$pvalue <= 0))
	expect_true(validObject(st))

})

