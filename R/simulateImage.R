
# Simulate a mass spectrometry imaging experiment
simulateImage <- function(pixelData, featureData,
	from = 0.9 * min(mz), to = 1.1 * max(mz), by = 400,
	sdrun = 1, sdpixel = 1, spcorr = 0.3, sptype = "SAR",
	representation = c("profile", "centroid"), units=c("ppm", "mz"),
	as = c("MSImagingExperiment", "SparseImagingExperiment"), ...)
{
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
		classes <- as.matrix(as.data.frame(pixelData[ii,,drop=FALSE], slots=FALSE))
		peaks <- as.matrix(as.data.frame(featureData[,nm,drop=FALSE], slots=FALSE))
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
	iData(data) <- iData(data) - min(iData(data))
	if ( representation == "centroid" )
		featureData(data)[names(featureData)] <- featureData
	if ( as == "SparseImagingExperiment" )
		data <- SparseImagingExperiment(iData(data),
			featureData=as(featureData(data), "XDataFrame"),
			pixelData=pixelData(data))
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

