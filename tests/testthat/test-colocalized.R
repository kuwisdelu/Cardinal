require(testthat)
require(Cardinal)

context("colocalized")

test_that("colocalized - SpectralImagingExperiment", {

	set.seed(1)
	s <- simulateImage(preset=2, dim=c(10L, 10L),
		representation="centroid")
	s$class <- makeFactor(circle=s$circle, square=s$square,
		bg=!s$circle & !s$square)

	co <- colocalized(s, i=1:2)
	co1 <- colocalized(s, mz=564.3)
	co2 <- colocalized(s, mz=603.7)
	co3 <- colocalized(s, i=3, sort.by="MOC")
	co4 <- colocalized(s, i=4, sort.by="Dice")
	co5 <- colocalized(s, ref=s$class, sort.by="none")

	expect_length(co, 2L)
	expect_equal(co[[1L]], co1)
	expect_equal(co[[2L]], co2)
	expect_false(is.unsorted(rev(co1$cor)))
	expect_false(is.unsorted(rev(co2$cor)))
	expect_false(is.unsorted(rev(co3$MOC)))
	expect_false(is.unsorted(rev(co4$Dice)))
	expect_length(co5, 3L)
	expect_equal(names(co5), levels(s$class))
	expect_false(is.unsorted(co5$mz))

})

