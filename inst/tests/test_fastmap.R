require(testthat)

context("fastmap")

test_that("Fastmap euclidean", {

	# Arithmetic example from Faloutsos and Lin

	d1 <- c(0, 1, 1, 100, 100)
	d2 <- c(1, 0, 1, 100, 100)
	d3 <- c(1, 1, 0, 100, 100)
	d4 <- c(100, 100, 100, 0, 1)
	d5 <- c(100, 100, 100, 1, 0)

	d <- rbind(d1, d2, d3, d4, d5)
	
	o1 <- c(1/sqrt(2), 0, 0, 0, 0, 0)
	o2 <- c(0, 1/sqrt(2), 0, 0, 0, 0)
	o3 <- c(0, 0, 1/sqrt(2), 0, 0, 0)
	o4 <- c(0, 0, 0, 0, sqrt(100^2 - (1/sqrt(2))^2 - (1/2)^2), 1/2)
	o5 <- c(0, 0, 0, 0, sqrt(100^2 - (1/sqrt(2))^2 - (1/2)^2), -1/2)

	x <- rbind(o1, o2, o3, o4, o5)

	expect_equivalent(as.matrix(dist(x)), d)

	proj <- fastmap(x, ncomp=5, scale=FALSE)

	expect_equivalent(dist(proj$scores), dist(x))

	f1 <- c(0, 0.005, 0.005, 100, 99.995)
	f2 <- c(0.707089, 1.41418, 1.06062, 0.707089, 0)
	f3 <- c(0.668149, 0.935411, 0, 0.668149, 1)

	f <- cbind(f1, f2, f3)

	# expect_equivalent(f, proj$scores[,1:3]) # why doesn't this work?

	p1 <- c(1, 4)
	p2 <- c(5, 2)
	p3 <- c(3, 5)

	p <- rbind(p1, p2, p3)

	# expect_equivalent(p, proj$pivot.array[1:3,]) # why doesn't this work?

})
