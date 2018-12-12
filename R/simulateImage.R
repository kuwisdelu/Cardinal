
# Simulate a mass spectrometry imaging experiment
simulateImage <- function(pixelData, featureData,
	from = 0.9 * min(mz), to = 1.1 * max(mz), by = 400,
	sdrun = 1, sdpixel = 1, spcorr = 0.3, sptype = "SAR", preset,
	representation = c("profile", "centroid"), units=c("ppm", "mz"),
	as = c("MSImagingExperiment", "SparseImagingExperiment"), ...)
{
	if ( !missing(preset) && (missing(pixelData) || missing(featureData)) ) {
		preset <- .presetImage(preset)
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

.presetImage <- function(i, nrow = 20, ncol = 20, npeaks = 20) {
	coords <- expand.grid(x=1:nrow, y=1:ncol)
	pdata_a <- PositionDataFrame(coords, run="sample1")
	pdata_b <- PositionDataFrame(coords, run="sample2")
	mzs <- sort(rlnorm(npeaks, 7, 0.3))
	diff <- 1 # difference in conditions
	peakheight <- 1 # peak heights
	i <- abs(i - 1) %% 9 + 1
	if ( i %in% c(1, 2) ) {
		# non-overlapping circle + square w/ 1 or 2 samples
		if ( i == 1 ) {
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
		n1 <- floor(npeaks / 2)
		n2 <- npeaks - n1
		fdata <- MassDataFrame(mz=mzs,
			circle=c(abs(rnorm(n1, peakheight)), rep(0, n2)),
			square=c(rep(0, n1), abs(rnorm(n2, peakheight))))
	} else if ( i %in% c(3, 4) ) {
		# overlapping circle + square w/ 1 or 2 samples
		if ( i == 3 ) {
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
		fdata <- MassDataFrame(mz=mzs,
			circle=c(abs(rnorm(n1 + n2, peakheight)), rep(0, n3)),
			square=c(rep(0, n1), abs(rnorm(n2 + n3, peakheight))))
	} else if ( i %in% c(5, 6) ) {
		# 2 squares + overlapping center circle w/ 1 or 2 samples
		if ( i == 5 ) {
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
		fdata <- MassDataFrame(mz=mzs,
			square1=c(abs(rnorm(n1 + n2, peakheight)), rep(0, n3)),
			square2=c(rep(0, n1), abs(rnorm(n2 + n3, peakheight))),
			circle=c(rep(0, n1), abs(rnorm(n2, peakheight)), rep(0, n3)))
	} else if ( i == 7 ) {
		# circle + square w/ diff conditions between 2 samples
		rx <- ncol / 4
		ry <- nrow / 4
		pdata_a <- addShape(pdata_a, center=c(x=rx, y=ry),
			size=(rx + ry) / 2, shape = "circle", name = "circle1")
		pdata_a <- addShape(pdata_a, center=c(x=rx * 3, y=ry * 3),
			size=(rx + ry) / 2, shape = "square", name = "square1")
		pdata_a[,c("circle2", "square2")] <- FALSE
		pdata_b <- addShape(pdata_b, center=c(x=rx, y=ry),
			size=(rx + ry) / 2, shape = "circle", name = "circle2")
		pdata_b <- addShape(pdata_b, center=c(x=rx * 3, y=ry * 3),
			size=(rx + ry) / 2, shape = "square", name = "square2")
		pdata_b[,c("circle1", "square1")] <- FALSE
		pdata <- rbind(pdata_a, pdata_b)
		n1 <- floor(npeaks / 3)
		n2 <- floor(npeaks / 3)
		n3 <- npeaks - n1 - n2
		fdata <- MassDataFrame(mz=mzs,
			circle1=c(abs(rnorm(n1, peakheight)),
				rep(peakheight, n2), rep(0, n3)),
			square1=c(abs(rnorm(n1, peakheight)),
				rep(0, n2), rep(peakheight, n3)),
			circle2=c(abs(rnorm(n1, peakheight)),
				rep(peakheight + diff, n2), rep(0, n3)),
			square2=c(abs(rnorm(n1, peakheight)),
				rep(0, n2), rep(peakheight + diff, n3)))
	} else if ( i == 8 ) {
		# 2 squares + overlapping circle w/ diff conditions (squares)
		rx <- ncol / 4
		ry <- nrow / 4
		pdata_a <- addShape(pdata_a, center=c(x=rx, y=ry),
			size=(rx + ry) / 2, shape = "square", name = "square1a")
		pdata_a <- addShape(pdata_a, center=c(x=rx * 3, y=ry * 3),
			size=(rx + ry) / 2, shape = "square", name = "square1b")
		pdata_a[,c("square2a", "square2b")] <- FALSE
		pdata_b <- addShape(pdata_b, center=c(x=rx, y=ry),
			size=(rx + ry) / 2, shape = "square", name = "square2a")
		pdata_b <- addShape(pdata_b, center=c(x=rx * 3, y=ry * 3),
			size=(rx + ry) / 2, shape = "square", name = "square2b")
		pdata_b[,c("square1a", "square1b")] <- FALSE
		pdata <- rbind(pdata_a, pdata_b)
		pdata <- addShape(pdata, center=c(x=rx * 2, y=ry * 2),
			size=(rx + ry) / 2, shape = "circle")
		n1 <- floor(npeaks / 3)
		n2 <- floor(npeaks / 3)
		n3 <- npeaks - n1 - n2
		fdata <- MassDataFrame(mz=mzs,
			square1a=c(abs(rnorm(n1, peakheight)),
				rep(peakheight, n2), rep(0, n3)),
			square1b=c(abs(rnorm(n1, peakheight)),
				rep(0, n2), rep(peakheight, n3)),
			square2a=c(abs(rnorm(n1, peakheight)),
				rep(peakheight + diff, n2), rep(0, n3)),
			square2b=c(abs(rnorm(n1, peakheight)),
				rep(0, n2), rep(peakheight + diff, n3)),
			circle=c(rep(0, n1), abs(rnorm(n2 + n3, peakheight))))
	} else if ( i == 9 ) {
		# 2 squares + overlapping circle w/ diff conditions (cicle)
		rx <- ncol / 4
		ry <- nrow / 4
		pdata_a <- addShape(pdata_a, center=c(x=rx * 2, y=ry * 2),
			size=(rx + ry) / 2, shape = "circle", name = "circle1")
		pdata_a[["circle2"]] <- FALSE
		pdata_b <- addShape(pdata_b, center=c(x=rx * 2, y=ry * 2),
			size=(rx + ry) / 2, shape = "circle", name = "circle2")
		pdata_b[["circle1"]] <- FALSE
		pdata <- rbind(pdata_a, pdata_b)
		pdata <- addShape(pdata, center=c(x=rx, y=ry),
			size=(rx + ry) / 2, shape = "square", name = "square1")
		pdata <- addShape(pdata, center=c(x=rx * 3, y=ry * 3),
			size=(rx + ry) / 2, shape = "square", name = "square2")
		n1 <- floor(npeaks / 3)
		n2 <- floor(npeaks / 3)
		n3 <- npeaks - n1 - n2
		fdata <- MassDataFrame(mz=mzs,
			circle1=c(rep(0, n1), rep(peakheight, n2 + n3)),
			circle2=c(rep(0, n1), rep(peakheight + diff, n2 + n3)),
			square1=c(abs(rnorm(n1 + n2, peakheight)), rep(0, n3)),
			square2=c(abs(rnorm(n1 + n2, peakheight)), rep(0, n3)))
	}
	list(pixelData=pdata, featureData=fdata)
}


