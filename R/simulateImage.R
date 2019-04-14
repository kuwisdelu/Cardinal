
# Simulate a mass spectrometry imaging experiment
simulateImage <- function(pixelData, featureData, preset,
	from = 0.9 * min(mz), to = 1.1 * max(mz), by = 400,
	sdrun = 1, sdpixel = 1, spcorr = 0.3, sptype = "SAR",
	representation = c("profile", "centroid"), units=c("ppm", "mz"),
	as = c("MSImagingExperiment", "SparseImagingExperiment"),
	BPPARAM = bpparam(), ...)
{
	if ( !missing(preset) && (missing(pixelData) || missing(featureData)) ) {
		preset <- presetImageDef(preset, ...)
		featureData <- preset$featureData
		pixelData <- preset$pixelData
	}
	pData <- pixelData[sapply(pixelData, is.logical, slots=FALSE)]
	fData <- featureData[sapply(featureData, is.numeric, slots=FALSE)]
	if ( !all(names(pData) %in% names(fData)) )
		stop("column names of pixelData and featureData do not match")
	mz <- mz(fData)
	units <- match.arg(units)
	representation <- match.arg(representation)
	as <- match.arg(as)
	m <- mz(from=from, to=to, by=by, units=units)
	nm <- names(pData)
	rngseeds <- generateRNGStreams(nlevels(run(pData)))
	data <- bpmapply(function(run, seed) {
		.message("simulating ", nrow(pData), " spectra for ", run)
		# set up RNG streams
		oseed <- getRNGStream()
		on.exit(setRNGStream(oseed))
		setRNGStream(seed)
		# extract run information
		ii <- run == run(pData)
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
	jitter = TRUE, ...)
{
	ncol <- unname(dim[1L])
	nrow <- unname(dim[2L])
	sdx <- jitter * ncol / 20
	sdy <- jitter * nrow / 20
	sdr <- jitter * sqrt(sdx * sdy)
	coords <- expand.grid(x=1:nrow, y=1:ncol)
	pdata <- PositionDataFrame(coords, run="run0")
	mzs <- sort(rlnorm(npeaks, 7, 0.3))
	i <- (abs(preset) - 1L) %% 9L + 1L
	if ( i == 1L ) {
		# centered circle
		rx <- ncol / 2
		ry <- nrow / 2
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
			circle=c(abs(rnorm(npeaks, peakheight))))
	} else if ( i == 2L ) {
		# topleft circle + bottomright square
		rx <- ncol / 4
		ry <- nrow / 4
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
			circle=c(abs(rnorm(n1 + n2, peakheight[1])), rep(0, n3)),
			square=c(rep(0, n1), abs(rnorm(n2 + n3, peakheight[2]))))
	} else if ( i == 3L ) {
		# 2 corner squares + centered circle
		rx <- ncol / 4
		ry <- nrow / 4
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
			square1=c(abs(rnorm(n1 + n2, peakheight[1])), rep(0, n3)),
			square2=c(rep(0, n1), abs(rnorm(n2 + n3, peakheight[2]))),
			circle=c(rep(0, n1), abs(rnorm(n2, peakheight[3])), rep(0, n3)))
	} else if ( i == 4L ) {
		# centered circle w/ diff conditions
		rx <- ncol / 2
		ry <- nrow / 2
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
		peakheight_circle <- abs(rnorm(npeaks, peakheight))
		peakdiff_circle <- diff * rep_len(peakdiff, npeaks)
		fdata <- MassDataFrame(mz=mzs,
			circleA=peakheight_circle,
			circleB=peakheight_circle + peakdiff_circle,
			diff=diff)
	} else if ( i == 5L ) {
		# topleft circle + bottomright square w/ diff conditions
		rx <- ncol / 4
		ry <- nrow / 4
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
		peakheight_circle <- c(abs(rnorm(n1 + n2, peakheight[1])), rep(0, n3))
		peakheight_square <- c(rep(0, n1), abs(rnorm(n2 + n3, peakheight[2])))
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
		rx <- ncol / 4
		ry <- nrow / 4
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
		peakheight_circle <- c(rep(0, n1), abs(rnorm(n2, peakheight[3])), rep(0, n3))
		peakdiff_circle <- diff.circle * rep_len(peakdiff, npeaks)
		fdata <- MassDataFrame(mz=mzs,
			square1=c(abs(rnorm(n1 + n2, peakheight[1])), rep(0, n3)),
			square2=c(rep(0, n1), abs(rnorm(n2 + n3, peakheight[2]))),
			circleA=peakheight_circle,
			circleB=peakheight_circle + peakdiff_circle,
			diff.circle=diff.circle)
	}
	list(pixelData=pdata, featureData=fdata)
}

