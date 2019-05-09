require(testthat)
require(Cardinal)

context("apply")

options(Cardinal.progress=FALSE, Cardinal.verbose=FALSE)

test_that("pixelApply", {

	data <- matrix(1:256, nrow=4)
	coord <- expand.grid(x=1:4, y=1:4, z=1:4)
	sset <- SImageSet(data=data, coord=coord)

	fData(sset)$fflag <- rep(c(TRUE, FALSE), 2)
	pData(sset)$pflag <- rep(c(TRUE, FALSE), 32)

	tmp <- pixelApply(sset, max, .feature.groups=fflag)
	expect_equivalent(pixelApply(sset, max, .feature=fflag), tmp["TRUE",])
	expect_equivalent(pixelApply(sset, function(x) max(x[fflag])), tmp["TRUE",])

	tmp2 <- pixelApply(sset, function(x) .Index)
	expect_equivalent(tmp2, pixels(sset))

	# tmp3 <- pixelApply(sset, function(x) coord(.Object)[.Index,])
	# expect_equivalent(t(tmp3), as.matrix(coord(sset)))

})

test_that("featureApply", {

	data <- matrix(1:256, nrow=4)
	coord <- expand.grid(x=1:4, y=1:4, z=1:4)
	sset <- SImageSet(data=data, coord=coord)

	fData(sset)$fflag <- rep(c(TRUE, FALSE), 2)
	pData(sset)$pflag <- rep(c(TRUE, FALSE), 32)

	tmp <- featureApply(sset, max, .pixel.groups=pflag)
	expect_equivalent(featureApply(sset, max, .pixel=pflag), tmp["TRUE",])
	expect_equivalent(featureApply(sset, function(x) max(x[pflag])), tmp["TRUE",])

})

