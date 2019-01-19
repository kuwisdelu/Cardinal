
# Simulate a mass spectrometry imaging experiment
simulateImage <- function(pixelData, featureData, preset,
	from = 0.9 * min(mz), to = 1.1 * max(mz), by = 400,
	sdrun = 1, sdpixel = 1, spcorr = 0.3, sptype = "SAR",
	representation = c("profile", "centroid"), units=c("ppm", "mz"),
	as = c("MSImagingExperiment", "SparseImagingExperiment"), ...)
{
	if ( !missing(preset) && (missing(pixelData) || missing(featureData)) ) {
		preset <- presetImageDef(preset, ...)
		featureData <- preset$featureData
		pixelData <- preset$pixelData
	}
	if ( !all(names(pixelData) %in% names(featureData)) )
		stop("column names of pixelData and featureData must match")
	if ( !all(sapply(pixelData, is.logical, slots=FALSE)) )
		stop("all columns of pixelData must be logical vectors")
	if ( !all(sapply(featureData, is.numeric, slots=FALSE)) )
		stop("all columns of featureData must be logical vectors")
	mz <- mz(featureData)
	units <- match.arg(units)
	representation <- match.arg(representation)
	as <- match.arg(as)
	m <- mz(from=from, to=to, by=by, units=units)
	nm <- names(pixelData)
	data <- lapply(runNames(pixelData), function(run) {
		ii <- run == run(pixelData)
		classes <- as.matrix(pixelData[ii,,drop=FALSE], slots=FALSE)
		peaks <- as.matrix(featureData[,nm,drop=FALSE], slots=FALSE)
		runerr <- rnorm(nrow(peaks), sd=sdrun)
		pixelerr <- rnorm(nrow(pixelData), sd=sdpixel)
		if ( spcorr > 0 ) {
			W <- as.matrix(spatialWeights(pixelData, r=1, matrix=TRUE))
			IrW <- as(diag(nrow(W)) - spcorr * W, "sparseMatrix")
			SARcov <- as(Matrix::solve(t(IrW) %*% IrW), "denseMatrix")
			SARcovL <- Matrix::chol((SARcov + t(SARcov))/2)
			pixelerr <- as.numeric(t(SARcovL) %*% pixelerr)
			pixelerr <- sdpixel * ((pixelerr - mean(pixelerr)) / sd(pixelerr))
		}
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
		if ( representation == "profile" ) {
			MSImagingExperiment(spectra,
				featureData=MassDataFrame(mz=m),
				pixelData=pixelData[ii,])
		} else {
			MSImagingExperiment(spectra,
				featureData=MassDataFrame(mz=mz),
				pixelData=pixelData[ii,])
		}
	})
	data <- do.call("cbind", data)
	# iData(data) <- iData(data) - min(iData(data))
	if ( representation == "centroid" )
		featureData(data)[names(featureData)] <- featureData
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

presetImageDef <- function(preset = 1L, npeaks = 30L,
	dim = c(20L, 20L), peakheight = 1, peakdiff = 1, ...)
{
	ncol <- unname(dim[1L])
	nrow <- unname(dim[2L])
	coords <- expand.grid(x=1:nrow, y=1:ncol)
	pdata_a <- PositionDataFrame(coords, run="sample1")
	pdata_b <- PositionDataFrame(coords, run="sample2")
	mzs <- sort(rlnorm(npeaks, 7, 0.3))
	i <- (abs(preset) - 1L) %% 9L + 1L
	if ( i %in% c(1L, 2L) ) {
		# non-overlapping circle + square w/ 1 or 2 samples
		if ( i == 1L ) {
			pdata <- pdata_a
		} else {
			pdata <- rbind(pdata_a, pdata_b)
		}
		rx <- ncol / 4
		ry <- nrow / 4
		pdata <- addShape(pdata, center=c(x=rx, y=ry),
			size=(rx + ry) / 2, shape = "circle")
		pdata <- addShape(pdata, center=c(x=rx * 3, y=ry * 3),
			size=(rx + ry) / 2, shape = "square")
		n1 <- floor(npeaks / 3)
		n2 <- floor(npeaks / 3)
		n3 <- npeaks - n1 - n2
		peakheight <- rep_len(peakheight, 2)
		fdata <- MassDataFrame(mz=mzs,
			circle=c(abs(rnorm(n1 + n2, peakheight[1])), rep(0, n3)),
			square=c(rep(0, n1), abs(rnorm(n2 + n3, peakheight[2]))))
	} else if ( i %in% c(3L, 4L) ) {
		# overlapping circle + square w/ 1 or 2 samples
		if ( i == 3L ) {
			pdata <- pdata_a
		} else {
			pdata <- rbind(pdata_a, pdata_b)
		}
		rx <- ncol / 3
		ry <- nrow / 3
		pdata <- addShape(pdata, center=c(x=rx, y=ry),
			size=(rx + ry) / 2, shape = "circle")
		pdata <- addShape(pdata, center=c(x=rx * 2, y=ry * 2),
			size=(rx + ry) / 2, shape = "square")
		n1 <- floor(npeaks / 3)
		n2 <- floor(npeaks / 3)
		n3 <- npeaks - n1 - n2
		peakheight <- rep_len(peakheight, 2)
		fdata <- MassDataFrame(mz=mzs,
			circle=c(abs(rnorm(n1 + n2, peakheight[1])), rep(0, n3)),
			square=c(rep(0, n1), abs(rnorm(n2 + n3, peakheight[2]))))
	} else if ( i %in% c(5L, 6L) ) {
		# 2 squares + overlapping center circle w/ 1 or 2 samples
		if ( i == 5L ) {
			pdata <- pdata_a
		} else {
			pdata <- rbind(pdata_a, pdata_b)
		}
		rx <- ncol / 4
		ry <- nrow / 4
		pdata <- addShape(pdata, center=c(x=rx, y=ry),
			size=(rx + ry) / 2, shape = "square", name = "square1")
		pdata <- addShape(pdata, center=c(x=rx * 3, y=ry * 3),
			size=(rx + ry) / 2, shape = "square", name = "square2")
		pdata <- addShape(pdata, center=c(x=rx * 2, y=ry * 2),
			size=(rx + ry) / 2, shape = "circle")
		n1 <- floor(npeaks / 3)
		n2 <- floor(npeaks / 3)
		n3 <- npeaks - n1 - n2
		peakheight <- rep_len(peakheight, 3)
		fdata <- MassDataFrame(mz=mzs,
			square1=c(abs(rnorm(n1 + n2, peakheight[1])), rep(0, n3)),
			square2=c(rep(0, n1), abs(rnorm(n2 + n3, peakheight[2]))),
			circle=c(rep(0, n1), abs(rnorm(n2, peakheight[3])), rep(0, n3)))
	} else if ( i == 7L ) {
		# circle + square w/ diff conditions between 2 samples
		rx <- ncol / 4
		ry <- nrow / 4
		pdata_a <- addShape(pdata_a, center=c(x=rx, y=ry),
			size=(rx + ry) / 2, shape = "circle", name = "circleA")
		pdata_a <- addShape(pdata_a, center=c(x=rx * 3, y=ry * 3),
			size=(rx + ry) / 2, shape = "square", name = "squareA")
		pdata_a[,c("circleB", "squareB")] <- FALSE
		pdata_b <- addShape(pdata_b, center=c(x=rx, y=ry),
			size=(rx + ry) / 2, shape = "circle", name = "circleB")
		pdata_b <- addShape(pdata_b, center=c(x=rx * 3, y=ry * 3),
			size=(rx + ry) / 2, shape = "square", name = "squareB")
		pdata_b[,c("circleA", "squareA")] <- FALSE
		pdata <- rbind(pdata_a, pdata_b)
		n1 <- floor(npeaks / 3)
		n2 <- floor(npeaks / 3)
		n3 <- npeaks - n1 - n2
		peakheight <- rep_len(peakheight, 2)
		fdata <- MassDataFrame(mz=mzs,
			circleA=c(abs(rnorm(n1, peakheight[1])),
				rep(peakheight[1], n2), rep(0, n3)),
			squareA=c(abs(rnorm(n1, peakheight[2])),
				rep(0, n2), rep(peakheight[2], n3)),
			circleB=c(abs(rnorm(n1, peakheight[1])),
				rep(peakheight[1] + peakdiff, n2), rep(0, n3)),
			squareB=c(abs(rnorm(n1, peakheight[2])),
				rep(0, n2), rep(peakheight[2] + peakdiff, n3)))
	} else if ( i == 8L ) {
		# 2 squares + overlapping circle w/ diff conditions (squares)
		rx <- ncol / 4
		ry <- nrow / 4
		pdata_a <- addShape(pdata_a, center=c(x=rx, y=ry),
			size=(rx + ry) / 2, shape = "square", name = "square1A")
		pdata_a <- addShape(pdata_a, center=c(x=rx * 3, y=ry * 3),
			size=(rx + ry) / 2, shape = "square", name = "square2A")
		pdata_a[,c("square1B", "square2B")] <- FALSE
		pdata_b <- addShape(pdata_b, center=c(x=rx, y=ry),
			size=(rx + ry) / 2, shape = "square", name = "square1B")
		pdata_b <- addShape(pdata_b, center=c(x=rx * 3, y=ry * 3),
			size=(rx + ry) / 2, shape = "square", name = "square2B")
		pdata_b[,c("square1A", "square2A")] <- FALSE
		pdata <- rbind(pdata_a, pdata_b)
		pdata <- addShape(pdata, center=c(x=rx * 2, y=ry * 2),
			size=(rx + ry) / 2, shape = "circle")
		n1 <- floor(npeaks / 3)
		n2 <- floor(npeaks / 3)
		n3 <- npeaks - n1 - n2
		peakheight <- rep_len(peakheight, 3)
		fdata <- MassDataFrame(mz=mzs,
			square1A=c(abs(rnorm(n1, peakheight[1])),
				rep(peakheight[1], n2), rep(0, n3)),
			square2A=c(abs(rnorm(n1, peakheight[2])),
				rep(0, n2), rep(peakheight[2], n3)),
			square1B=c(abs(rnorm(n1, peakheight[1])),
				rep(peakheight[1] + peakdiff, n2), rep(0, n3)),
			square2B=c(abs(rnorm(n1, peakheight[2])),
				rep(0, n2), rep(peakheight[2] + peakdiff, n3)),
			circle=c(rep(0, n1), abs(rnorm(n2 + n3, peakheight[3]))))
	} else if ( i == 9L ) {
		# 2 squares + overlapping circle w/ diff conditions (cicle)
		rx <- ncol / 4
		ry <- nrow / 4
		pdata_a <- addShape(pdata_a, center=c(x=rx * 2, y=ry * 2),
			size=(rx + ry) / 2, shape = "circle", name = "circleA")
		pdata_a[["circleB"]] <- FALSE
		pdata_b <- addShape(pdata_b, center=c(x=rx * 2, y=ry * 2),
			size=(rx + ry) / 2, shape = "circle", name = "circleB")
		pdata_b[["circleA"]] <- FALSE
		pdata <- rbind(pdata_a, pdata_b)
		pdata <- addShape(pdata, center=c(x=rx, y=ry),
			size=(rx + ry) / 2, shape = "square", name = "square1")
		pdata <- addShape(pdata, center=c(x=rx * 3, y=ry * 3),
			size=(rx + ry) / 2, shape = "square", name = "square2")
		n1 <- floor(npeaks / 3)
		n2 <- floor(npeaks / 3)
		n3 <- npeaks - n1 - n2
		peakheight <- rep_len(peakheight, 3)
		fdata <- MassDataFrame(mz=mzs,
			circleA=c(rep(0, n1), rep(peakheight[1], n2 + n3)),
			circleB=c(rep(0, n1), rep(peakheight[1] + peakdiff, n2 + n3)),
			square1=c(abs(rnorm(n1 + n2, peakheight[2])), rep(0, n3)),
			square2=c(abs(rnorm(n1 + n2, peakheight[3])), rep(0, n3)))
	}
	list(pixelData=pdata, featureData=fdata)
}

