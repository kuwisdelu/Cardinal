
#### Simulate mass spectra ####
## ----------------------------

simulateSpectra <- function(n = 1L, npeaks = 50L,
	mz = rlnorm(npeaks, 7, 0.3), intensity = rlnorm(npeaks, 1, 0.9),
	from = 0.9 * min(mz), to = 1.1 * max(mz), by = 400,
	sdpeaks = sdpeakmult * log1p(intensity), sdpeakmult = 0.2,
	sdnoise = 0.1, sdmz = 10, resolution = 1000, fmax = 0.5,
	baseline = 0, decay = 10, units=c("ppm", "mz"),
	centroided = FALSE, ...)
{
	if ( "peaks" %in% ...names() ) {
		.Deprecated(old="peaks", new="npeaks")
		npeaks <- list(...)$peaks
	}
	if ( "representation" %in% ...names() ) {
		.Deprecated(old="representation", new="centroided")
		centroided <- list(...)$representation == "centroided"
	}
	if ( length(mz) != length(intensity) )
		.Error("length of mz and intensity must match")
	units <- match.arg(units)
	if ( missing(mz) && (!missing(from) || !missing(to)) ) {
		mz <- (mz - min(mz)) / max(mz - min(mz))
		mz <- (from + 0.1 * (to - from)) + (0.8 * (to - from)) * mz
	}
	metadata <- list(centroided=centroided)
	x <- as.vector(mz(from=from, to=to, by=by, units=units))
	y <- simspec(n=n,
		x=mz, y=intensity, domain=x,
		sdx=switch(units, ppm=1e-6 * sdmz, mz=sdmz),
		sdy=sdpeaks, sdymult=sdpeakmult, sdnoise=sdnoise,
		resolution=resolution, fmax=fmax,
		baseline=baseline, decay=decay,
		units=switch(units, ppm="relative", mz="absolute"))
	if ( centroided ) {
		y <- apply(as.matrix(y), 2L,
			function(yi) {
				approx1(x, yi, mz, interp="max",
					tol=0.5 * min(diff(mz)), tol.ref="abs")
			})
	} else {
		dm <- dim(y)
		metadata$peaks <- attr(y, "design")$x
		attributes(y) <- NULL
		dim(y) <- dm
	}
	ans <- MassDataFrame(mz=x, intensity=I(y))
	metadata(ans) <- metadata
	ans
}

simulateSpectrum <- function(...)
{
	.Deprecated("simulateSpectra")
	simulateSpectra(...)
}


#### Simulate mass spectrometry image ####
## ---------------------------------------

simulateImage <- function(pixelData, featureData, preset,
	from = 0.9 * min(mz), to = 1.1 * max(mz), by = 400,
	sdrun = 1, sdpixel = 1, spcorr = 0.3, units=c("ppm", "mz"),
	centroided = FALSE, continuous = TRUE,
	verbose = getCardinalVerbose(), chunkopts = list(),
	BPPARAM = getCardinalBPPARAM(), ...)
{
	if ( "representation" %in% ...names() ) {
		.Deprecated(old="representation", new="centroided")
		centroided <- list(...)$representation == "centroided"
	}
	if ( !"L'Ecuyer-CMRG" %in% RNGkind() )
		.Warn("use RNGkind(\"L'Ecuyer-CMRG\")",
			" for statistically safe RNG")
	if ( !missing(preset) && !is.null(preset) ) {
		preset <- presetImageDef(preset, ...)
		featureData <- preset$featureData
		pixelData <- preset$pixelData
	} else {
		if ( missing(pixelData) || missing(featureData) )
			.Error("must provide either 'preset' or ",
				"both of 'pixelData' and 'featureData'")
		preset <- NULL
	}
	mz <- mz(featureData)
	if ( !is.null(preset) && (!missing(from) || !missing(to)) ) {
		mz <- (mz - min(mz)) / max(mz - min(mz))
		mz <- (from + 0.1 * (to - from)) + (0.8 * (to - from)) * mz
		mz(featureData) <- mz
	}
	if ( nrun(pixelData) > 1L ) {
		return(.simulateImages(pixelData=pixelData, featureData=featureData,
			from=force(from), to=force(to), by=force(by),
			sdrun=sdrun, sdpixel=sdpixel, spcorr=spcorr, units=units,
			centroided=centroided, continuous=continuous,
			verbose=verbose, chunkopts=chunkopts,
			BPPARAM=BPPARAM, ...))
	}
	# get shared design columns
	design <- list(pixelData=pixelData, featureData=featureData)
	scols <- intersect(names(pixelData), names(featureData))
	if ( length(scols) == 0L )
		.Error("no shared column names between pixelData and featureData")
	pdata <- pixelData[scols]
	fdata <- featureData[scols]
	# compute domain
	units <- match.arg(units)
	domain <- mz(from=from, to=to, by=by, units=units)
	.Log("simulating mass range ",
		round(from, digits=4L), " to ", round(to, digits=4L),
		" with ", round(by, digits=6L), " ", units, " resolution",
		message=verbose)
	# compute run information
	intensity <- as.matrix(fdata)
	runerr <- rnorm(nrow(fdata), sd=sdrun)
	pixelerr <- rnorm(nrow(pdata), sd=sdpixel)
	# calculate spatial autoregressive (SAR) covariance
	if ( spcorr > 0 ) {
		.Log("simulating spatial covariance for ", nrow(pixelData), " pixels",
			message=verbose)
		W <- as.matrix(spatialWeights(as.matrix(coord(pixelData)),
			r=1, weights="gaussian", matrix=TRUE, verbose=FALSE))
		IrW <- as(diag(nrow(W)) - spcorr * W, "sparseMatrix")
		SARcov <- as(Matrix::solve(crossprod(IrW, IrW)), "denseMatrix")
		SARcovL <- Matrix::chol((SARcov + t(SARcov)) / 2)
		pixelerr <- as.numeric(t(SARcovL) %*% pixelerr)
		pixelerr <- sdpixel * ((pixelerr - mean(pixelerr)) / sd(pixelerr))
	}
	# simulate run
	.Log("simulating ", length(domain), " intensities ",
		"and ", length(mz), " peaks per pixel",
		message=verbose)
	group <- as.matrix(pdata)
	SIMULATE <- isofun(function(i, group, mz, intensity,
		from, to, by, units, runerr, pixelerr, ...)
	{
		present <- group[i,,drop=TRUE]
		if ( any(present) ) {
			x <- rowSums(intensity[,present,drop=FALSE])
		} else {
			x <- rep.int(0, length(mz))
		}
		x <- pmax(0, x + runerr + pixelerr[i])
		simulateSpectra(1L, mz=mz, intensity=x,
			from=from, to=to, by=by, units=units,
			centroided=FALSE, ...)$intensity
	}, CardinalEnv())
	spectra <- chunkLapply(seq_len(nrow(group)), SIMULATE,
		group=group, mz=mz, intensity=intensity, units=units,
		from=from, to=to, by=by,, runerr=runerr, pixelerr=pixelerr,
		verbose=verbose, chunkopts=chunkopts,
		BPPARAM=BPPARAM, ...)
	# process spectra
	if ( continuous ) {
		if ( centroided ) {
			# continuous, centroided
			spectra <- vapply(spectra,
				function(s) {
					approx1(domain, s, mz, interp="max",
						tol=0.5 * min(diff(mz)), tol.ref="abs")
				}, numeric(length(mz)))
		} else {
			# continuous, profile
			mz <- domain
			spectra <- do.call(cbind, spectra)
			featureData <- MassDataFrame(mz=mz)
		}
		MSImagingExperiment(spectra,
			featureData=featureData,
			pixelData=pixelData,
			centroided=centroided,
			metadata=list(design=design))
	} else {
		if ( centroided ) {
			# processed, centroided
			peaks <- lapply(spectra, findpeaks)
			mz <- lapply(peaks, function(p) domain[p])
			spectra <- Map(`[`, spectra, peaks)
		} else {
			# processed, profile
			mz <- rep.int(list(domain), length(spectra))
		}
		MSImagingArrays(list(intensity=spectra, mz=mz),
			pixelData=pixelData,
			centroided=centroided,
			metadata=list(design=design))
	}
}

.simulateImages <- function(pixelData, featureData,
	verbose = getCardinalVerbose(), chunkopts = list(),
	BPPARAM = getCardinalBPPARAM(), ...)
{
	design <- list(pixelData=pixelData, featureData=featureData)
	runs <- vector("list", length=nrun(pixelData))
	runs <- setNames(runs, runNames(pixelData))
	for ( i in seq_len(nrun(pixelData)) )
	{
		irun <- runNames(pixelData)[i]
		pdata <- pixelData[run(pixelData) %in% irun,]
		.Log("simulating run ", i, "/", nrun(pixelData),
			" (", nrow(pdata), " spectra | ", sQuote(irun), ")",
			message=verbose)
		runs[[irun]] <- simulateImage(pdata, featureData,
			verbose=verbose, chunkopts=chunkopts,
			BPPARAM=BPPARAM, ...)
	}
	runData <- lapply(runs, function(run) metadata(run)$design$pixelData)
	design$pixelData <- do.call(rbind, runData)
	if ( is(runs[[1L]], "MSImagingExperiment") ) {
		runs <- do.call(cbind, runs)
		if ( centroided(runs) )
			featureData(runs) <- featureData
	} else {
		runs <- do.call(c, runs)
	}
	metadata(runs) <- list(design=design)
	runs
}


#### Simulation presets ####
## --------------------------

addShape <- function(pixelData, center, size,
	shape = c("circle", "square"), name = shape)
{
	shape <- match.arg(shape)
	coord <- as.matrix(coord(pixelData))
	if ( length(center) != ncol(coord) ) {
		center <- rep_len(unname(center), ncol(coord))
	} else if ( !is.null(names(center)) ) {
		center <- center[colnames(coord)]
	}
	in_shape <- apply(coord, 1L, function(pos) {
		switch(shape,
			circle=sqrt(sum((pos - center)^2)) <= size,
			square=all(abs(pos - center) <= size))
	})
	pixelData[[name]] <- in_shape
	pixelData
}

presetImageDef <- function(preset = 1L, nrun = 1, npeaks = 30L,
	dim = c(20L, 20L), peakheight = exp(1), peakdiff = exp(1),
	sdsample = 0.2, jitter = TRUE, ...)
{
	numPresets <- 9L
	nx <- unname(dim[1L])
	ny <- unname(dim[2L])
	sdx <- jitter * nx / 20
	sdy <- jitter * ny / 20
	sdr <- jitter * sqrt(sdx * sdy)
	coords <- expand.grid(x=seq_len(nx), y=seq_len(ny))
	pdata <- PositionDataFrame(coord=coords, run="run0")
	mzs <- sort(rlnorm(npeaks, 7, 0.3))
	i <- (abs(preset) - 1L) %% numPresets + 1L
	.Log("using image preset = ", i,
		message=getCardinalVerbose())
	if ( i == 1L ) {
		# centered circle
		rx <- nx / 2
		ry <- ny / 2
		pdata <- lapply(seq_len(nrun), function(i) {
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
			circle=c(pmax(0, rnorm(npeaks, peakheight, sd=sdsample))))
	} else if ( i == 2L ) {
		# topleft circle + bottomright square
		rx <- nx / 4
		ry <- ny / 4
		pdata <- lapply(seq_len(nrun), function(i) {
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
			circle=c(pmax(0, rnorm(n1 + n2, peakheight[1], sd=sdsample)), rep(0, n3)),
			square=c(rep(0, n1), pmax(0, rnorm(n2 + n3, peakheight[2], sd=sdsample))))
	} else if ( i == 3L ) {
		# 2 corner squares + centered circle
		rx <- nx / 4
		ry <- ny / 4
		pdata <- lapply(seq_len(nrun), function(i) {
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
			square1=c(pmax(0, rnorm(n1 + n2, peakheight[1], sd=sdsample)), rep(0, n3)),
			square2=c(rep(0, n1), pmax(0, rnorm(n2 + n3, peakheight[2], sd=sdsample))),
			circle=c(rep(0, n1), pmax(0, rnorm(n2, peakheight[3], sd=sdsample)), rep(0, n3)))
	} else if ( i == 4L ) {
		# centered circle w/ diff conditions
		rx <- nx / 2
		ry <- ny / 2
		pdata_a <- lapply(seq_len(nrun), function(i) {
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
		pdata_b <- lapply(seq_len(nrun), function(i) {
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
		peakheight_circle <- pmax(0, rnorm(npeaks, peakheight, sd=sdsample))
		peakdiff_circle <- diff * rep_len(peakdiff, npeaks)
		fdata <- MassDataFrame(mz=mzs,
			circleA=peakheight_circle,
			circleB=peakheight_circle + peakdiff_circle,
			diff=diff)
	} else if ( i == 5L ) {
		# topleft circle + bottomright square w/ diff conditions
		rx <- nx / 4
		ry <- ny / 4
		pdata_a <- lapply(seq_len(nrun), function(i) {
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
		pdata_b <- lapply(seq_len(nrun), function(i) {
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
		peakheight_circle <- c(pmax(0, rnorm(n1 + n2, peakheight[1], sd=sdsample)), rep(0, n3))
		peakheight_square <- c(rep(0, n1), pmax(0, rnorm(n2 + n3, peakheight[2], sd=sdsample)))
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
		pdata <- lapply(seq_len(nrun), function(i) {
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
		}, pdata, seq_len(nrun), SIMPLIFY=FALSE)
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
		}, pdata, seq_len(nrun), SIMPLIFY=FALSE)
		pdata_b <- do.call("rbind", pdata_b)
		pdata_b$condition <- "B"
		pdata <- rbind(pdata_a, pdata_b)
		pdata$condition <- as.factor(pdata$condition)
		n1 <- floor(npeaks / 3)
		n2 <- floor(npeaks / 3)
		n3 <- npeaks - n1 - n2
		peakheight <- rep_len(peakheight, 3)
		diff.circle <- c(rep(FALSE, n1), rep(TRUE, n2), rep(FALSE, n3))
		peakheight_circle <- c(rep(0, n1), pmax(0, rnorm(n2, peakheight[3], sd=sdsample)), rep(0, n3))
		peakdiff_circle <- diff.circle * rep_len(peakdiff, npeaks)
		fdata <- MassDataFrame(mz=mzs,
			square1=c(pmax(0, rnorm(n1 + n2, peakheight[1], sd=sdsample)), rep(0, n3)),
			square2=c(rep(0, n1), pmax(0, rnorm(n2 + n3, peakheight[2], sd=sdsample))),
			circleA=peakheight_circle,
			circleB=peakheight_circle + peakdiff_circle,
			diff.circle=diff.circle)
	} else if ( i == 7L ) {
		# pairs of circles w/ diff conditions + ref peak
		rx <- nx / 4
		ry <- ny / 4
		pdata <- lapply(seq_len(nrun), function(i) {
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
		peakheight_circle <- c(pmax(0, rnorm(n1 + n2, peakheight[1], sd=sdsample)), rep(0, n3))
		peakdiff_circle <- diff * rep_len(peakdiff, npeaks)
		fdata <- MassDataFrame(mz=mzs,
			ref=c(rep(0, n1 + n2), pmax(0, rnorm(n3, peakheight[2], sd=sdsample))),
			circleA=peakheight_circle,
			circleB=peakheight_circle + peakdiff_circle,
			diff=diff)
	} else if ( i == 8L ) {
		# pairs of circles + squares w/ diff conditions + ref peak
		rx <- nx / 4
		ry <- ny / 4
		pdata <- lapply(seq_len(nrun), function(i) {
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
		peakheight_circle <- c(pmax(0, rnorm(n1 + n2, peakheight[2], sd=sdsample)), rep(0, n3))
		peakdiff_circle <- diff.circle * rep_len(peakdiff, npeaks)
		peakheight_square <- c(pmax(0, rnorm(n1 + n2, peakheight[1], sd=sdsample)), rep(0, n3))
		peakdiff_square <- diff.square * rep_len(peakdiff, npeaks)
		fdata <- MassDataFrame(mz=mzs,
			ref=c(rep(0, n1 + n2), pmax(0, rnorm(n3, peakheight[3], sd=sdsample))),
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
		runscales <- sqrt(1 + abs(seq_len(nrun) - ceiling(nrun / 2)))
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
		}, runscales, seq_len(nrun))
		pdata <- do.call("rbind", pdata)
		n1 <- floor(npeaks / 3)
		n2 <- floor(npeaks / 3)
		n3 <- npeaks - n1 - n2
		peakheight <- rep_len(peakheight, 2)
		fdata <- MassDataFrame(mz=mzs,
			sphere1=c(pmax(0, rnorm(n1 + n2, peakheight[1], sd=sdsample)), rep(0, n3)),
			sphere2=c(rep(0, n1), pmax(0, rnorm(n2 + n3, peakheight[2], sd=sdsample))))
	}
	list(pixelData=pdata, featureData=fdata)
}

