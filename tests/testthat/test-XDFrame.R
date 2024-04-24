require(testthat)
require(Cardinal)

context("XDataFrame")

test_that("XDFrame accessors", {

	expect_true(validObject(XDataFrame()))

	set.seed(1)
	d0 <- DataFrame(A=sort(runif(10)), B=1:10, C=11:20, D=letters[1:10])
	d1 <- XDataFrame(d0, keys=list(index1="A", index2=c("B", "C")))

	expect_true(validObject(d1))

	d1$X <- 10:1

	expect_equal(d1$X, 10:1)
	expect_equal(keys(d1, "index1"), d1$A)
	expect_equal(keys(d1, "index1", drop=FALSE), d1["A"])
	expect_equal(keys(d1, "index2"), d1[c("B", "C")])
	expect_equal(keys(d1, 1L), keys(d1, "index1"))
	expect_equal(keys(d1, 2L), keys(d1, "index2"))
	expect_equivalent(class(keys(d1, "index2")), "DFrame")
	expect_equivalent(class(d1[c("B", "C")]), "DFrame")

	d1[c("Y", "Z")] <- list(20:11, 30:21)

	expect_equal(d1$Y, 20:11)
	expect_equal(d1$Z, 30:21)

	d1[c("X", "Y", "Z")] <- NULL

	expect_null(d1$X)
	expect_null(d1$Y)
	expect_null(d1$Z)

	A2 <- sort(runif(10))

	keys(d1, "index1") <- A2

	expect_equal(d1$A, keys(d1, "index1"))
	expect_equal(d1$A, A2)

	keys(d1, "index2") <- list(E=d1$B, F=d1$C)

	expect_equal(keys(d1, "index2"), d1[c("E", "F")])
	expect_equal(d1$E, d1$B)
	expect_equal(d1$F, d1$C)

	keys(d1, "index2")$E <- 10:1

	expect_equal(keys(d1, "index2"), d1[c("E", "F")])

	expect_error(d1$E <- NULL)

	keys(d1)$index2 <- c("D", "F")

	expect_true(validObject(d1))
	expect_equal(keys(d1)$index2, c("D", "F"))
	expect_equal(keys(d1, "index2"), d1[c("D", "F")])

	names(d1)[c(4L,6L)] <- c("X", "Y")

	expect_true(validObject(d1))
	expect_equal(names(d1), c("A", "B", "C", "X", "E", "Y"))
	expect_equal(keys(d1)$index2, c("X", "Y"))
	expect_equal(keys(d1, "index2"), d1[c("X", "Y")])

})

test_that("XDFrame rbind/cbind", {

	set.seed(1)
	d0 <- DataFrame(A=sort(runif(10)), B=1:10, C=11:20, D=letters[1:10])
	d1 <- XDataFrame(d0, keys=list(index1="A", index2=c("B", "C")))

	c00 <- cbind(d0, d0)
	c11 <- cbind(d1, d1)

	expect_true(validObject(c11))
	expect_is(c11, "XDFrame")
	expect_equal(keys(d1), keys(c11))
	expect_equal(cbind(d0, d0["D"]), as(c11, "DFrame"))

	r00 <- rbind(d0, d0)
	r11 <- rbind(d1, d1)
	
	expect_true(validObject(r11))
	expect_is(r11, "XDFrame")
	expect_equal(keys(d1), keys(r11))
	expect_equal(r00, as(r11, "DFrame"))

	d2 <- XDataFrame(d0, keys=list(index1="A"))

	c12 <- cbind(d1, d2)
	c21 <- cbind(d2, d1)

	expect_equal(c11, c12)
	expect_equal(c11, c21)

	r12 <- rbind(d1, d2)
	r21 <- rbind(d2, d1)

	expect_equal(r11, r12)
	expect_equal(r11, r21)

	keys(d2, "index1") <- sort(runif(10))

	expect_error(cbind(d1, d2))

})

test_that("PositionDataFrame", {

	expect_true(validObject(PositionDataFrame()))

	Coord <- expand.grid(x=1:9, y=1:9)
	n <- nrow(Coord)
	d0 <- PositionDataFrame(coord=Coord, A=seq_len(n), B=rev(seq_len(n)))

	expect_true(validObject(d0))
	expect_equal(coord(d0), DataFrame(Coord))
	expect_equal(coord(d0, "x"), Coord$x)
	expect_equal(coord(d0, "y"), Coord$y)
	expect_equal(coord(d0, 1L), Coord$x)
	expect_equal(coord(d0, 2L), Coord$y)
	expect_equal(coordNames(d0), c("x", "y"))
	
	coordNames(d0) <- c("X", "Y")

	expect_true(validObject(d0))
	expect_equal(names(coord(d0)), c("X", "Y"))
	expect_equal(coordNames(d0), c("X", "Y"))

	coord(d0)[] <- -Coord

	expect_true(validObject(d0))
	expect_equal(names(coord(d0)), c("X", "Y"))
	expect_equal(coordNames(d0), c("X", "Y"))
	expect_equal(coord(d0, "X"), -Coord$x)
	expect_equal(coord(d0, "Y"), -Coord$y)

	coord(d0) <- Coord

	expect_true(validObject(d0))
	expect_equal(coord(d0), DataFrame(Coord))
	expect_equal(coordNames(d0), c("x", "y"))

	coord(d0, "x") <- max(coord(d0, "x")) - coord(d0, "x") + 1L

	expect_equal(coord(d0, "x"), max(Coord$x) - Coord$x + 1L)
	expect_equal(coord(d0, "x"), d0$x)

	coord(d0)$x <- max(coord(d0)$x) - coord(d0)$x + 1L

	expect_equal(coord(d0, "x"), Coord$x)
	expect_equal(coord(d0, "x"), d0$x)

	Run <- rep.int(factor("run1"), n)
	d1 <- PositionDataFrame(coord=Coord, run=Run, A=seq_len(n), B=rev(seq_len(n)))

	expect_true(validObject(d1))
	expect_equal(nrun(d1), 1L)
	expect_equal(run(d1), Run)
	expect_equal(runNames(d1), "run1")
	expect_equal(runNames(d1), levels(run(d1)))

	run(d1) <- "run2"

	expect_true(validObject(d1))
	expect_equal(runNames(d1), "run2")
	expect_equal(runNames(d1), levels(run(d1)))

	runNames(d1) <- "run3"

	expect_true(validObject(d1))
	expect_equal(runNames(d1), "run3")
	expect_equal(runNames(d1), levels(run(d1)))

	d2 <- d1
	runNames(d2) <- "run4"
	d3 <- rbind(d1, d2)

	expect_setequal(runNames(d3), c("run3", "run4"))
	expect_setequal(runNames(head(d3, 6L)), "run3")
	expect_setequal(runNames(tail(d3, 6L)), "run4")

	df <- DataFrame(expand.grid(x=1:3, y=1:3), A=1:9)
	expect_true(validObject(as(df, "PositionDataFrame")))

	d4 <- PositionDataFrame(coord=Coord)
	coord(d4)$z <- 1

	expect_equal(coordNames(d4), c("x", "y", "z"))

	coord(d4)$z <- NULL

	expect_equal(coordNames(d4), c("x", "y"))

})

test_that("PositionDataFrame rbind/cbind", {

	Coord <- expand.grid(x=1:9, y=1:9)
	Coord2 <- expand.grid(x=11:19, y=11:19)
	n <- nrow(Coord)
	d0 <- PositionDataFrame(coord=Coord, A=seq_len(n), B=rev(seq_len(n)))
	d1 <- d0
	coord(d1) <- Coord2

	r00 <- rbind(d0, d0)
	r01 <- rbind(d0, d1)

	expect_true(validObject(r00))
	expect_true(validObject(r01))
	expect_equal(r00$x, c(Coord$x, Coord$x))
	expect_equal(r00$y, c(Coord$y, Coord$y))
	expect_equal(r01$x, c(Coord$x, Coord2$x))
	expect_equal(r01$y, c(Coord$y, Coord2$y))

	expect_error(c01 <- cbind(d0, d1))

})

test_that("MassDataFrame", {

	expect_true(validObject(MassDataFrame()))

	n <- 100
	mz <- seq(500, 510, length.out=n)
	d0 <- MassDataFrame(mz=mz, A=seq_len(n), B=rev(seq_len(n)))

	expect_true(validObject(d0))
	expect_equal(mz(d0), mz)
	expect_equal(mz(d0), d0$mz)
	
	mz2 <- seq(700, 710, length.out=n)
	mz(d0) <- mz2

	expect_true(validObject(d0))
	expect_equal(mz(d0), mz2)
	expect_equal(mz(d0), d0$mz)

	mz3 <- rev(mz2)

	expect_error(mz(d0) <- mz3)

	d1 <- d0[1:10,]

	expect_is(d1, "MassDataFrame")
	expect_true(validObject(d1))

	d2 <- d0[10:1,]

	expect_is(d2, "DataFrame")
	expect_true(validObject(d2))

	df <- DataFrame(mz=seq(500, 505, by=0.1))
	expect_true(validObject(as(df, "MassDataFrame")))

})

test_that("MassDataFrame rbind/cbind", {

	n <- 100
	mz <- seq(500, 510, length.out=n)
	mz2 <- seq(511, 520, length.out=n)
	d0 <- MassDataFrame(mz=mz, A=seq_len(n), B=rev(seq_len(n)))
	d1 <- d0
	mz(d1) <- mz2

	expect_error(r00 <- rbind(d0, d0))
	r01 <- rbind(d0, d1)

	expect_true(validObject(r01))
	expect_equal(r01$mz, c(mz, mz2))

	expect_error(c01 <- cbind(d0, d1))

})
