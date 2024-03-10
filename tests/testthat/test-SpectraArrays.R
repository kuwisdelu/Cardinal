
context("SpectraArrays")

test_that("SpectraArrays 1-D", {

	expect_true(validObject(SpectraArrays()))

	s <- SpectraArrays(1:10)
	expect_true(validObject(s))
	expect_equal(s[[1L]], 1:10)

	a0 <- 1:10
	a1 <- 11:20
	a2 <- 21:30
	al <- list(a0=a0, a1=a1, a2=a2)
	s <- SpectraArrays(al)

	expect_true(validObject(s))
	expect_equal(names(s), names(al))
	expect_length(s, 3L)
	expect_equal(s[["a0"]], a0)
	expect_equal(s[["a1"]], a1)
	expect_equal(s[["a2"]], a2)
	expect_equal(s[[1L]], a0)
	expect_equal(s[[2L]], a1)
	expect_equal(s[[3L]], a2)

	nms <- c("x", "y", "z")
	names(s) <- nms

	expect_true(validObject(s))
	expect_equal(names(s), nms)
	expect_equal(s[["x"]], a0)
	expect_equal(s[["y"]], a1)
	expect_equal(s[["z"]], a2)

	s2 <- s[1:5]

	expect_true(validObject(s))
	expect_equal(s2[[1L]], a0[1:5])
	expect_equal(s2[[2L]], a1[1:5])
	expect_equal(s2[[3L]], a2[1:5])

	a0[1:5] <- 101:105
	a1[1:5] <- 101:105
	a2[1:5] <- 101:105
	s[1:5] <- rep.int(list(101:105), 3)

	expect_true(validObject(s))
	expect_equal(s[[1L]], a0)
	expect_equal(s[[2L]], a1)
	expect_equal(s[[3L]], a2)

	sc <- cbind(s, s)

	expect_true(validObject(sc))
	expect_equivalent(sc[[1L]], cbind(a0, a0))
	expect_equivalent(sc[[2L]], cbind(a1, a1))
	expect_equivalent(sc[[3L]], cbind(a2, a2))

	sr <- rbind(s, s)

	expect_true(validObject(sr))
	expect_equivalent(sr[[1L]], rbind(a0, a0))
	expect_equivalent(sr[[2L]], rbind(a1, a1))
	expect_equivalent(sr[[3L]], rbind(a2, a2))

	l0 <- list(1:10, 11:20, 21:30, 31:40, 41:50)
	l1 <- Map("*", l0, 1.01)
	l2 <- Map("*", l0, 1.11)
	ll <- list(l0=l0, l1=l1, l2=l2)
	sl <- SpectraArrays(ll)

	expect_true(validObject(sl))
	expect_equal(names(sl), names(ll))
	expect_length(sl, 3L)
	expect_equal(sl[["l0"]], l0)
	expect_equal(sl[["l1"]], l1)
	expect_equal(sl[["l2"]], l2)
	expect_equal(sl[[1L]], l0)
	expect_equal(sl[[2L]], l1)
	expect_equal(sl[[3L]], l2)

	slc <- c(sl, sl)

	expect_true(validObject(slc))
	expect_equivalent(slc[[1L]], c(l0, l0))
	expect_equivalent(slc[[2L]], c(l1, l1))
	expect_equivalent(slc[[3L]], c(l2, l2))

})

test_that("SpectraArrays 2-D", {

	s <- SpectraArrays(diag(5))
	expect_true(validObject(s))
	expect_equal(s[[1L]], diag(5))

	a0 <- matrix(1:25, nrow=5, ncol=5)
	a1 <- matrix(26:50, nrow=5, ncol=5)
	a2 <- matrix(51:75, nrow=5, ncol=5)
	al <- list(a0=a0, a1=a1, a2=a2)
	s <- SpectraArrays(al)

	expect_true(validObject(s))
	expect_equal(names(s), names(al))
	expect_length(s, 3L)
	expect_equal(s[["a0"]], a0)
	expect_equal(s[["a1"]], a1)
	expect_equal(s[["a2"]], a2)
	expect_equal(s[[1L]], a0)
	expect_equal(s[[2L]], a1)
	expect_equal(s[[3L]], a2)

	nms <- c("x", "y", "z")
	names(s) <- nms

	expect_true(validObject(s))
	expect_equal(names(s), nms)
	expect_equal(s[["x"]], a0)
	expect_equal(s[["y"]], a1)
	expect_equal(s[["z"]], a2)

	s2 <- s[1:3,1:3]

	expect_true(validObject(s2))
	expect_equal(s2[[1L]], a0[1:3,1:3])
	expect_equal(s2[[2L]], a1[1:3,1:3])
	expect_equal(s2[[3L]], a2[1:3,1:3])

	s3 <- s[1:3,]

	expect_true(validObject(s3))
	expect_equal(s3[[1L]], a0[1:3,])
	expect_equal(s3[[2L]], a1[1:3,])
	expect_equal(s3[[3L]], a2[1:3,])

	s4 <- s[,1:3]

	expect_true(validObject(s4))
	expect_equal(s4[[1L]], a0[,1:3])
	expect_equal(s4[[2L]], a1[,1:3])
	expect_equal(s4[[3L]], a2[,1:3])

	a0[1:3,1:3] <- diag(3)
	a1[1:3,1:3] <- diag(3)
	a2[1:3,1:3] <- diag(3)
	s[1:3,1:3] <- rep.int(list(diag(3)), 3)

	expect_true(validObject(s))
	expect_equal(s[[1L]], a0)
	expect_equal(s[[2L]], a1)
	expect_equal(s[[3L]], a2)

	a0[1:3,] <- 100
	a1[1:3,] <- 200
	a2[1:3,] <- 300
	s[1:3,] <- list(100, 200, 300)

	expect_true(validObject(s))
	expect_equal(s[[1L]], a0)
	expect_equal(s[[2L]], a1)
	expect_equal(s[[3L]], a2)

	a0[,1:3] <- -100
	a1[,1:3] <- -200
	a2[,1:3] <- -300
	s[,1:3] <- list(-100, -200, -300)

	expect_true(validObject(s))
	expect_equal(s[[1L]], a0)
	expect_equal(s[[2L]], a1)
	expect_equal(s[[3L]], a2)

	sc <- cbind(s, s)

	expect_true(validObject(sc))
	expect_equivalent(sc[[1L]], cbind(a0, a0))
	expect_equivalent(sc[[2L]], cbind(a1, a1))
	expect_equivalent(sc[[3L]], cbind(a2, a2))

	sr <- rbind(s, s)

	expect_true(validObject(sr))
	expect_equivalent(sr[[1L]], rbind(a0, a0))
	expect_equivalent(sr[[2L]], rbind(a1, a1))
	expect_equivalent(sr[[3L]], rbind(a2, a2))

})
