
# Simulate a mass spectrum
simulateSpectra <- function(n = 1L, npeaks = 50L,
	mz = rlnorm(npeaks, 7, 0.3), intensity = rlnorm(npeaks, 1, 0.9),
	from = 0.9 * min(mz), to = 1.1 * max(mz), by = 400,
	sdpeaks = sdpeakmult * log1p(intensity), sdpeakmult = 0.2,
	sdnoise = 0.1, sdmz = 10, resolution = 1000, fmax = 0.5,
	baseline = 0, decay = 10, units=c("ppm", "mz"),
	representation = c("profile", "centroid"), ...)
{
	if ( "peaks" %in% ...names() ) {
		.Deprecated("npeaks")
		npeaks <- list(...)$peaks
	}
	if ( length(mz) != length(intensity) )
		stop("length of mz and intensity must match")
	units <- match.arg(units)
	representation <- match.arg(representation)
	if ( missing(mz) && (!missing(from) || !missing(to)) ) {
		mz <- (mz - min(mz)) / max(mz - min(mz))
		mz <- (from + 0.1 * (to - from)) + (0.8 * (to - from)) * mz
	}
	x <- as.vector(mz(from=from, to=to, by=by, units=units))
	y <- simspec(n=n, x=mz, y=intensity,
		domain=x, sdx=switch(units, ppm=1e-6 * sdmz, mz=sdmz),
		sdy=sdnoise, sdymult=sdpeaks, resolution=resolution,
		fmax=fmax, baseline=baseline, decay=decay,
		units=switch(units, ppm="relative", mz="absolute"))
	if ( representation == "centroid" ) {
		FUN <- function(yi) approx(domain, yi, mz)$y
		y <- apply(y, 2L, FUN)
		ans <- list(mz=mz, intensity=y)
	} else {
		peaks <- attr(y, "peaks")
		attr(y, "peaks") <- NULL
		attr(y, "domain") <- NULL
		ans <- list(mz=x, intensity=y, peaks=peaks)
	}
	ans
}

simulateSpectrum <- function(...)
{
	.Deprecated("simulateSpectra")
	simulateSpectra(...)
}

# Simulate a mass spectrometry imaging experiment
simulateImage <- function(pixelData, featureData, preset,
	from = 0.9 * min(mz), to = 1.1 * max(mz), by = 400,
	sdrun = 1, sdpixel = 1, spcorr = 0.3, sptype = "SAR",
	representation = c("profile", "centroid"), units=c("ppm", "mz"),
	as = c("MSImagingExperiment", "SparseImagingExperiment"),
	BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( !missing(preset) ) {
		preset <- presetImageDef(preset, ...)
		featureData <- preset$featureData
		pixelData <- preset$pixelData
	}
	pData <- pixelData[sapply(pixelData, is.logical, slots=FALSE)]
	fData <- featureData[sapply(featureData, is.numeric, slots=FALSE)]
	if ( !all(names(pData) %in% names(fData)) )
		stop("column names of pixelData and featureData do not match")
	mz <- mz(fData)
	if ( (!missing(from) || !missing(to)) && !missing(preset) ) {
		mz <- (mz - min(mz)) / max(mz - min(mz))
		mz <- (from + 0.1 * (to - from)) + (0.8 * (to - from)) * mz
		mz(fData) <- mz(featureData) <- mz
	}
	units <- match.arg(units)
	representation <- match.arg(representation)
	as <- match.arg(as)
	m <- mz(from=from, to=to, by=by, units=units)
	nm <- names(pData)
	rngseeds <- generateRNGStreams(nlevels(run(pData)))
	data <- bpmapply(function(run, seed) {
		ii <- run == run(pData)
		.message("simulating ", sum(ii), " spectra for ", run)
		# set up RNG streams
		oseed <- getRNGStream()
		on.exit(setRNGStream(oseed))
		setRNGStream(seed)
		# extract run information
		classes <- as.matrix(pData[ii,,drop=FALSE], slots=FALSE)
		peaks <- as.matrix(fData[,nm,drop=FALSE], slots=FALSE)
		runerr <- rnorm(nrow(peaks), sd=sdrun)
		pixelerr <- rnorm(nrow(pData), sd=sdpixel)
		# calculate spatial covariance
		if ( spcorr > 0 ) {
			W <- as.matrix(spatialWeights(pData, r=1, matrix=TRUE))
			IrW <- as(diag(nrow(W)) - spcorr * W, "sparseMatrix")
			SARcov <- as(Matrix::solve(t(IrW) %*% IrW), "denseMatrix")
			SARcovL <- Matrix::chol((SARcov + t(SARcov))/2)
			pixelerr <- as.numeric(t(SARcovL) %*% pixelerr)
			pixelerr <- sdpixel * ((pixelerr - mean(pixelerr)) / sd(pixelerr))
		}
		# simulate spectra
		spectra <- sapply(1:nrow(classes), function(i) {
			j <- classes[i,]
			if ( any(j) ) {
				intensity <- rowSums(peaks[,j,drop=FALSE])
			} else {
				intensity <- rep(0, nrow(peaks))
			}
			nz <- intensity != 0
			intensity[nz] <- intensity[nz] + runerr[nz] + pixelerr[i]
			intensity <- pmax(intensity, 0)
			simulateSpectrum(mz=mz, intensity=intensity,
				from=from, to=to, by=by, units=units,
				representation=representation, ...)$intensity
		})
		# return data
		if ( representation == "profile" ) {
			MSImagingExperiment(spectra,
				featureData=MassDataFrame(mz=m),
				pixelData=pixelData[ii,])
		} else {
			MSImagingExperiment(spectra,
				featureData=MassDataFrame(mz=mz),
				pixelData=pixelData[ii,])
		}
	}, runNames(pData), rngseeds, SIMPLIFY=FALSE, BPPARAM=BPPARAM)
	data <- do.call("cbind", data)
	if ( representation == "centroid" ) {
		featureData(data)[names(featureData)] <- featureData
		centroided(data) <- TRUE
	}
	if ( as == "SparseImagingExperiment" )
		data <- SparseImagingExperiment(iData(data),
			featureData=as(featureData(data), "XDataFrame"),
			pixelData=pixelData(data))
	design <- list(pixelData=pixelData, featureData=featureData)
	metadata(data)[["design"]] <- design
	data
}

addShape <- function(pixelData, center, size,
	shape=c("circle", "square"), name=shape)
{
	shape <- match.arg(shape)
	coord <- as.matrix(coord(pixelData))
	if ( length(center) != ncol(coord) ) {
		center <- rep_len(unname(center), ncol(coord))
	} else if ( !is.null(names(center)) ) {
		center <- center[colnames(coord)]
	}
	isShape <- apply(coord, 1, function(pos) {
		if ( shape == "circle" ) {
			sqrt(sum((pos - center)^2)) <= size
		} else if ( shape == "square" ) {
			all(abs(pos - center) <= size)
		}
	})
	pixelData[[name]] <- isShape
	pixelData
}

presetImageDef <- function(preset = 1L, nruns = 1, npeaks = 30L,
	dim = c(20L, 20L), peakheight = 1, peakdiff = 1,
	sdsample = 0.2, jitter = TRUE, ...)
{
	numPresets <- 9L
	nx <- unname(dim[1L])
	ny <- unname(dim[2L])
	sdx <- jitter * nx / 20
	sdy <- jitter * ny / 20
	sdr <- jitter * sqrt(sdx * sdy)
	coords <- expand.grid(x=1:nx, y=1:ny)
	pdata <- PositionDataFrame(coords, run="run0")
	mzs <- sort(rlnorm(npeaks, 7, 0.3))
	i <- (abs(preset) - 1L) %% numPresets + 1L
	.message("using image preset = ", i)
	if ( i == 1L ) {
		# centered circle
		rx <- nx / 2
		ry <- ny / 2
		pdata <- lapply(1:nruns, function(i) {
			pdata_i <- pdata
			pdata_i <- addShape(pdata_i,
				center=c(
					x=rx + rnorm(1, sd=sdx),
					y=ry + rnorm(1, sd=sdy)),
				size=(rx + ry) / 3 + rnorm(1, sd=sdr),
				shape = "circle")
			runNames(pdata_i) <- paste0("run", i-1)
			pdata_i
		})
		pdata <- do.call("rbind", pdata)
		fdata <- MassDataFrame(mz=mzs,
			circle=c(pos(rnorm(npeaks, peakheight, sd=sdsample))))
	} else if ( i == 2L ) {
		# topleft circle + bottomright square
		rx <- nx / 4
		ry <- ny / 4
		pdata <- lapply(1:nruns, function(i) {
			pdata_i <- pdata
			pdata_i <- addShape(pdata_i,
				center=c(
					x=rx + rnorm(1, sd=sdx),
					y=ry + rnorm(1, sd=sdy)),
				size=(rx + ry) / 2 + rnorm(1, sd=sdr),
				shape = "circle")
			pdata_i <- addShape(pdata_i,
				center=c(
					x=rx * 3 + rnorm(1, sd=sdx),
					y=ry * 3 + rnorm(1, sd=sdy)),
				size=(rx + ry) / 2  + rnorm(1, sd=sdr),
				shape = "square")
			runNames(pdata_i) <- paste0("run", i-1)
			pdata_i
		})
		pdata <- do.call("rbind", pdata)
		n1 <- floor(npeaks / 3)
		n2 <- floor(npeaks / 3)
		n3 <- npeaks - n1 - n2
		peakheight <- rep_len(peakheight, 2)
		fdata <- MassDataFrame(mz=mzs,
			circle=c(pos(rnorm(n1 + n2, peakheight[1], sd=sdsample)), rep(0, n3)),
			square=c(rep(0, n1), pos(rnorm(n2 + n3, peakheight[2], sd=sdsample))))
	} else if ( i == 3L ) {
		# 2 corner squares + centered circle
		rx <- nx / 4
		ry <- ny / 4
		pdata <- lapply(1:nruns, function(i) {
			pdata_i <- pdata
			pdata_i <- addShape(pdata_i,
				center=c(
					x=rx + rnorm(1, sd=sdx),
					y=ry + rnorm(1, sd=sdy)),
				size=(rx + ry) / 2 + rnorm(1, sd=sdr),
				shape = "square", name = "square1")
			pdata_i <- addShape(pdata_i,
				center=c(
					x=rx * 3 + rnorm(1, sd=sdx),
					y=ry * 3 + rnorm(1, sd=sdy)),
				size=(rx + ry) / 2  + rnorm(1, sd=sdr),
				shape = "square", name = "square2")
			pdata_i <- addShape(pdata_i,
				center=c(
					x=rx * 2 + rnorm(1, sd=sdx),
					y=ry * 2 + rnorm(1, sd=sdy)),
				size=(rx + ry) / 2  + rnorm(1, sd=sdr),
				shape = "circle")
			runNames(pdata_i) <- paste0("run", i-1)
			pdata_i
		})
		pdata <- do.call("rbind", pdata)
		n1 <- floor(npeaks / 3)
		n2 <- floor(npeaks / 3)
		n3 <- npeaks - n1 - n2
		peakheight <- rep_len(peakheight, 3)
		fdata <- MassDataFrame(mz=mzs,
			square1=c(pos(rnorm(n1 + n2, peakheight[1], sd=sdsample)), rep(0, n3)),
			square2=c(rep(0, n1), pos(rnorm(n2 + n3, peakheight[2], sd=sdsample))),
			circle=c(rep(0, n1), pos(rnorm(n2, peakheight[3], sd=sdsample)), rep(0, n3)))
	} else if ( i == 4L ) {
		# centered circle w/ diff conditions
		rx <- nx / 2
		ry <- ny / 2
		pdata_a <- lapply(1:nruns, function(i) {
			pdata_i <- pdata
			pdata_i <- addShape(pdata_i,
				center=c(
					x=rx + rnorm(1, sd=sdx),
					y=ry + rnorm(1, sd=sdy)),
				size=(rx + ry) / 3 + rnorm(1, sd=sdr),
				shape = "circle", name="circleA")
			pdata_i[,"circleB"] <- FALSE
			runNames(pdata_i) <- paste0("runA", i)
			pdata_i
		})
		pdata_a <- do.call("rbind", pdata_a)
		pdata_a$condition <- "A"
		pdata_b <- lapply(1:nruns, function(i) {
			pdata_i <- pdata
			pdata_i <- addShape(pdata_i,
				center=c(
					x=rx + rnorm(1, sd=sdx),
					y=ry + rnorm(1, sd=sdy)),
				size=(rx + ry) / 3 + rnorm(1, sd=sdr),
				shape = "circle", name="circleB")
			pdata_i[,"circleA"] <- FALSE
			runNames(pdata_i) <- paste0("runB", i)
			pdata_i
		})
		pdata_b <- do.call("rbind", pdata_b)
		pdata_b$condition <- "B"
		pdata <- rbind(pdata_a, pdata_b)
		pdata$condition <- as.factor(pdata$condition)
		n1 <- floor(npeaks / 2)
		n2 <- npeaks - n1
		diff <- c(rep(TRUE, n1), rep(FALSE, n2))
		peakheight_circle <- pos(rnorm(npeaks, peakheight, sd=sdsample))
		peakdiff_circle <- diff * rep_len(peakdiff, npeaks)
		fdata <- MassDataFrame(mz=mzs,
			circleA=peakheight_circle,
			circleB=peakheight_circle + peakdiff_circle,
			diff=diff)
	} else if ( i == 5L ) {
		# topleft circle + bottomright square w/ diff conditions
		rx <- nx / 4
		ry <- ny / 4
		pdata_a <- lapply(1:nruns, function(i) {
			pdata_i <- pdata
			pdata_i <- addShape(pdata_i,
				center=c(
					x=rx + rnorm(1, sd=sdx),
					y=ry + rnorm(1, sd=sdy)),
				size=(rx + ry) / 2 + rnorm(1, sd=sdr),
				shape = "circle", name = "circleA")
			pdata_i <- addShape(pdata_i,
				center=c(
					x=rx * 3 + rnorm(1, sd=sdx),
					y=ry * 3 + rnorm(1, sd=sdy)),
				size=(rx + ry) / 2  + rnorm(1, sd=sdr),
				shape = "square", name = "squareA")
			pdata_i[,"circleB"] <- FALSE
			pdata_i[,"squareB"] <- FALSE
			runNames(pdata_i) <- paste0("runA", i)
			pdata_i
		})
		pdata_a <- do.call("rbind", pdata_a)
		pdata_a$condition <- "A"
		pdata_b <- lapply(1:nruns, function(i) {
			pdata_i <- pdata
			pdata_i <- addShape(pdata_i,
				center=c(
					x=rx + rnorm(1, sd=sdx),
					y=ry + rnorm(1, sd=sdy)),
				size=(rx + ry) / 2 + rnorm(1, sd=sdr),
				shape = "circle", name = "circleB")
			pdata_i <- addShape(pdata_i,
				center=c(
					x=rx * 3 + rnorm(1, sd=sdx),
					y=ry * 3 + rnorm(1, sd=sdy)),
				size=(rx + ry) / 2  + rnorm(1, sd=sdr),
				shape = "square", name = "squareB")
			pdata_i[,"circleA"] <- FALSE
			pdata_i[,"squareA"] <- FALSE
			runNames(pdata_i) <- paste0("runB", i)
			pdata_i
		})
		pdata_b <- do.call("rbind", pdata_b)
		pdata_b$condition <- "B"
		pdata <- rbind(pdata_a, pdata_b)
		pdata$condition <- as.factor(pdata$condition)
		n1 <- floor(npeaks / 3)
		n2 <- floor(npeaks / 3)
		n3 <- npeaks - n1 - n2
		peakheight <- rep_len(peakheight, 2)
		diff.circle <- c(rep(TRUE, n1), rep(FALSE, n2 + n3))
		diff.square <- c(rep(FALSE, n1 + n2), rep(TRUE, n3))
		peakheight_circle <- c(pos(rnorm(n1 + n2, peakheight[1], sd=sdsample)), rep(0, n3))
		peakheight_square <- c(rep(0, n1), pos(rnorm(n2 + n3, peakheight[2], sd=sdsample)))
		peakdiff_circle <- diff.circle * rep_len(peakdiff, npeaks)
		peakdiff_square <- diff.square * rep_len(peakdiff, npeaks)
		fdata <- MassDataFrame(mz=mzs,
			circleA=peakheight_circle,
			circleB=peakheight_circle + peakdiff_circle,
			squareA=peakheight_square,
			squareB=peakheight_square + peakdiff_square,
			diff.circle=diff.circle, diff.square=diff.square)
	}  else if ( i == 6L ) {
		# 2 corner squares + centered circle w/ diff conditions
		rx <- nx / 4
		ry <- ny / 4
		pdata <- lapply(1:nruns, function(i) {
			pdata_i <- pdata
			pdata_i <- addShape(pdata_i,
				center=c(
					x=rx + rnorm(1, sd=sdx),
					y=ry + rnorm(1, sd=sdy)),
				size=(rx + ry) / 2 + rnorm(1, sd=sdr),
				shape = "square", name = "square1")
			pdata_i <- addShape(pdata_i,
				center=c(
					x=rx * 3 + rnorm(1, sd=sdx),
					y=ry * 3 + rnorm(1, sd=sdy)),
				size=(rx + ry) / 2  + rnorm(1, sd=sdr),
				shape = "square", name = "square2")
			pdata_i
		})
		pdata_a <- mapply(function(pdata_i, i) {
			pdata_i <- addShape(pdata_i,
				center=c(
					x=rx * 2 + rnorm(1, sd=sdx),
					y=ry * 2 + rnorm(1, sd=sdy)),
				size=(rx + ry) / 2  + rnorm(1, sd=sdr),
				shape = "circle", name = "circleA")
			pdata_i[,"circleB"] <- FALSE
			runNames(pdata_i) <- paste0("runA", i)
			pdata_i
		}, pdata, 1:nruns, SIMPLIFY=FALSE)
		pdata_a <- do.call("rbind", pdata_a)
		pdata_a$condition <- "A"
		pdata_b <- mapply(function(pdata_i, i) {
			pdata_i <- addShape(pdata_i,
				center=c(
					x=rx * 2 + rnorm(1, sd=sdx),
					y=ry * 2 + rnorm(1, sd=sdy)),
				size=(rx + ry) / 2  + rnorm(1, sd=sdr),
				shape = "circle", name = "circleB")
			pdata_i[,"circleA"] <- FALSE
			runNames(pdata_i) <- paste0("runB", i)
			pdata_i
		}, pdata, 1:nruns, SIMPLIFY=FALSE)
		pdata_b <- do.call("rbind", pdata_b)
		pdata_b$condition <- "B"
		pdata <- rbind(pdata_a, pdata_b)
		pdata$condition <- as.factor(pdata$condition)
		n1 <- floor(npeaks / 3)
		n2 <- floor(npeaks / 3)
		n3 <- npeaks - n1 - n2
		peakheight <- rep_len(peakheight, 3)
		diff.circle <- c(rep(FALSE, n1), rep(TRUE, n2), rep(FALSE, n3))
		peakheight_circle <- c(rep(0, n1), pos(rnorm(n2, peakheight[3], sd=sdsample)), rep(0, n3))
		peakdiff_circle <- diff.circle * rep_len(peakdiff, npeaks)
		fdata <- MassDataFrame(mz=mzs,
			square1=c(pos(rnorm(n1 + n2, peakheight[1], sd=sdsample)), rep(0, n3)),
			square2=c(rep(0, n1), pos(rnorm(n2 + n3, peakheight[2], sd=sdsample))),
			circleA=peakheight_circle,
			circleB=peakheight_circle + peakdiff_circle,
			diff.circle=diff.circle)
	} else if ( i == 7L ) {
		# pairs of circles w/ diff conditions + ref peak
		rx <- nx / 4
		ry <- ny / 4
		pdata <- lapply(1:nruns, function(i) {
			pdata_i <- pdata
			pdata_i <- addShape(pdata_i,
				center=c(
					x=rx + rnorm(1, sd=sdx),
					y=ry + rnorm(1, sd=sdy)),
				size=(rx + ry) / 2 + rnorm(1, sd=sdr),
				shape = "circle", name = "circleA")
			pdata_i <- addShape(pdata_i,
				center=c(
					x=rx * 3 + rnorm(1, sd=sdx),
					y=ry * 3 + rnorm(1, sd=sdy)),
				size=(rx + ry) / 2  + rnorm(1, sd=sdr),
				shape = "circle", name = "circleB")
			runNames(pdata_i) <- paste0("run", i-1)
			pdata_i
		})
		pdata <- do.call("rbind", pdata)
		pdata$ref <- TRUE
		conditionA <- (coord(pdata)$x <= nx / 2) &  (coord(pdata)$y <= ny / 2)
		conditionB <- (coord(pdata)$x >= nx / 2) &  (coord(pdata)$y >= ny / 2)
		pdata$condition <- makeFactor(A=conditionA, B=conditionB)
		n1 <- floor(npeaks / 3)
		n2 <- floor(npeaks / 3)
		n3 <- npeaks - n1 - n2
		peakheight <- rep_len(peakheight, 2)
		diff <- c(rep(TRUE, n1), rep(FALSE, n2), rep(FALSE, n3))
		peakheight_circle <- c(pos(rnorm(n1 + n2, peakheight[1], sd=sdsample)), rep(0, n3))
		peakdiff_circle <- diff * rep_len(peakdiff, npeaks)
		fdata <- MassDataFrame(mz=mzs,
			ref=c(rep(0, n1 + n2), pos(rnorm(n3, peakheight[2], sd=sdsample))),
			circleA=peakheight_circle,
			circleB=peakheight_circle + peakdiff_circle,
			diff=diff)
	} else if ( i == 8L ) {
		# pairs of circles + squares w/ diff conditions + ref peak
		rx <- nx / 4
		ry <- ny / 4
		pdata <- lapply(1:nruns, function(i) {
			pdata_i <- pdata
			pdata_i <- addShape(pdata_i,
				center=c(
					x=rx + rnorm(1, sd=sdx),
					y=ry + rnorm(1, sd=sdy)),
				size=(rx + ry) / 1.5 + rnorm(1, sd=sdr),
				shape = "square", name = "squareA")
			pdata_i <- addShape(pdata_i,
				center=c(
					x=rx * 3 + rnorm(1, sd=sdx),
					y=ry * 3 + rnorm(1, sd=sdy)),
				size=(rx + ry) / 1.5  + rnorm(1, sd=sdr),
				shape = "square", name = "squareB")
			pdata_i <- addShape(pdata_i,
				center=c(
					x=rx + rnorm(1, sd=sdx),
					y=ry + rnorm(1, sd=sdy)),
				size=(rx + ry) / 2.5 + rnorm(1, sd=sdr),
				shape = "circle", name = "circleA")
			pdata_i <- addShape(pdata_i,
				center=c(
					x=rx * 3 + rnorm(1, sd=sdx),
					y=ry * 3 + rnorm(1, sd=sdy)),
				size=(rx + ry) / 2.5  + rnorm(1, sd=sdr),
				shape = "circle", name = "circleB")
			runNames(pdata_i) <- paste0("run", i-1)
			pdata_i
		}) 
		pdata <- do.call("rbind", pdata)
		pdata$ref <- TRUE
		pdata$condition <- makeFactor(A=pdata$squareA, B=pdata$squareB)
		n1 <- floor(npeaks / 3)
		n2 <- floor(npeaks / 3)
		n3 <- npeaks - n1 - n2
		peakheight <- rep_len(peakheight, 3)
		diff.circle <- c(rep(TRUE, n1), rep(FALSE, n2), rep(FALSE, n3))
		diff.square <- c(rep(FALSE, n1), rep(TRUE, n2), rep(FALSE, n3))
		peakheight_circle <- c(pos(rnorm(n1 + n2, peakheight[2], sd=sdsample)), rep(0, n3))
		peakdiff_circle <- diff.circle * rep_len(peakdiff, npeaks)
		peakheight_square <- c(pos(rnorm(n1 + n2, peakheight[1], sd=sdsample)), rep(0, n3))
		peakdiff_square <- diff.square * rep_len(peakdiff, npeaks)
		fdata <- MassDataFrame(mz=mzs,
			ref=c(rep(0, n1 + n2), pos(rnorm(n3, peakheight[3], sd=sdsample))),
			squareA=peakheight_square,
			squareB=peakheight_square + peakdiff_square,
			circleA=peakheight_circle,
			circleB=peakheight_circle + peakdiff_circle,
			diff.circle=diff.circle,
			diff.square=diff.square)
	} else if ( i == 9L ) {
		# small spheres inside large sphere (3D)
		rx <- nx / 2
		ry <- ny / 2
		runscales <- sqrt(1 + abs(1:nruns - ceiling(nruns / 2)))
		pdata <- mapply(function(scale, i) {
			pdata_i <- pdata
			pdata_i <- addShape(pdata_i,
				center=c(
					x=rx + rnorm(1, sd=sdx),
					y=ry + rnorm(1, sd=sdy)),
				size=(rx + ry) / (scale * 2) + rnorm(1, sd=sdr),
				shape = "circle", name = "sphere1")
			pdata_i <- addShape(pdata_i,
				center=c(
					x=rx + rnorm(1, sd=sdx),
					y=ry + rnorm(1, sd=sdy)),
				size=(rx + ry) / (scale * 4) + rnorm(1, sd=sdr),
				shape = "circle", name = "sphere2")
			runNames(pdata_i) <- paste0("run", i-1)
			coord(pdata_i)$z <- i
			pdata_i
		}, runscales, 1:nruns)
		pdata <- do.call("rbind", pdata)
		n1 <- floor(npeaks / 3)
		n2 <- floor(npeaks / 3)
		n3 <- npeaks - n1 - n2
		peakheight <- rep_len(peakheight, 2)
		fdata <- MassDataFrame(mz=mzs,
			sphere1=c(pos(rnorm(n1 + n2, peakheight[1], sd=sdsample)), rep(0, n3)),
			sphere2=c(rep(0, n1), pos(rnorm(n2 + n3, peakheight[2], sd=sdsample))))
	}
	list(pixelData=pdata, featureData=fdata)
}

