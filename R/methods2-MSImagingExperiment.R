
#### Methods for MSImagingExperiment ####
## ------------------------------------------

MSImagingExperiment <- function(imageData = matrix(nrow=0, ncol=0),
	featureData = MassDataFrame(), pixelData = PositionDataFrame(),
	metadata = list(), processing = SimpleList(), centroided=FALSE)
{
	if ( is.matrix(imageData) || is(imageData, "matter_matc") ) {
		imageData <- MSContinuousImagingSpectraList(imageData)
	} else if ( is(imageData, "sparse_matc") ) {
		imageData <- MSProcessedImagingSpectraList(imageData)
	} else if ( !is(imageData, "ImageArrayList") ) {
		imageData <- ImageArrayList(imageData)
		if ( is.null(names(imageData)) ) {
			inames <- .format.numbered("intensity", length(imageData))
			names(imageData) <- inames
		}
	}
	if ( length(imageData) != 0L ) {
		iData <- imageData[[1]]
		if ( missing(featureData) ) {
			rownames <- rownames(iData)
			featureData <- new("DataFrame", nrows=nrow(iData),
				rownames=rownames)
		}
		if ( missing(pixelData) ) {
			colnames <- colnames(iData)
			coord=expand.grid(x=seq_len(ncol(iData)), y=1L)
			pixelData <- PositionDataFrame(coord=coord,
				row.names=colnames)
		}
	}
	if ( is(imageData, "MSContinuousImagingSpectraList") ) {
		.MSContinuousImagingExperiment(
			imageData=imageData,
			featureData=featureData,
			elementMetadata=pixelData,
			metadata=metadata,
			processing=processing,
			centroided=centroided)
	} else if ( is(imageData, "MSProcessedImagingSpectraList") ) {
		.MSProcessedImagingExperiment(
			imageData=imageData,
			featureData=featureData,
			elementMetadata=pixelData,
			metadata=metadata,
			processing=processing,
			centroided=centroided)
	} else if ( is(imageData, "ImageArrayList") ) {
		.MSImagingExperiment(
			imageData=imageData,
			featureData=featureData,
			elementMetadata=pixelData,
			metadata=metadata,
			processing=processing,
			centroided=centroided)
	}
}

## MS-related methods

setMethod("mz", "MSImagingExperiment", function(object, ...) mz(object@featureData))

setReplaceMethod("mz", "MSImagingExperiment",
	function(object, value) {
		mz(object@featureData) <- value
		object
	})

setMethod("spectra", "MSImagingExperiment", function(object, ...) iData(object, ...))

setReplaceMethod("spectra", "MSImagingExperiment",
	function(object, ..., value) {
		iData(object, ...) <- value
		object
	})

setMethod("centroided", "MSImagingExperiment", function(object) object@centroided)

setReplaceMethod("centroided", "MSImagingExperiment",
	function(object, value) {
		object@centroided <- value
		object
	})

setMethod("peaks", "MSImagingExperiment",
	function(object, ...) {
		if ( centroided(object) ) {
			iData(object, ...)
		} else {
			NULL
		}
	})

setReplaceMethod("peaks", "MSImagingExperiment",
	function(object, ..., value) {
		if ( centroided(object) ) {
			iData(object, ...) <- value
		} else {
			.stop("object must be 'centroided'")
		}
		object
	})

setMethod("resolution", "MSImagingExperiment",
	function(object) resolution(featureData(object)))

setReplaceMethod("resolution", "MSImagingExperiment",
	function(object, value) {
		resolution(featureData(object)) <- value
		object
	})

## Filter pixels/features

setMethod("features", "MSImagingExperiment",
	function(object, ..., mz, .env = parent.frame(1)) {
		if ( missing(mz) ) {
			features <- callNextMethod(object, ..., .env=.env)
		} else {
			mz <- as.numeric(mz)
			mzrange <- range(mz(object))
			outofrange <- mz < floor(mzrange[1]) | mz > ceiling(mzrange[2])
			if ( any(outofrange) )
				.warning("mz out of range: ", paste0(mz[outofrange], collapse=" "))
			features <- bsearch(mz, mz(object), nearest=TRUE)
			features <- features[!outofrange]
			if ( length(match.call(expand.dots=FALSE)$...) > 0 ) {
				keep <- features %in% callNextMethod(object, ..., .env=.env)
				features <- features[keep]
			}
			names(features) <- featureNames(object)[features]
		}
		features
	})

setMethod("pixels", "MSImagingExperiment",
	function(object, ..., coord, run, .env = parent.frame(1)) {
		if ( missing(coord) && missing(run) ) {
			pixels <- callNextMethod(object, ..., .env=.env)
		} else {
			if ( !missing(coord) ) {
				if ( !gridded(object) )
					.warning("pixel coordinates are not gridded")
				if ( is.null(names(coord)) )
					names(coord) <- names(coord(object))
				coord <- as.data.frame(as.list(coord))
				i1 <- unlist(apply(coord, 1, function(xy) {
					lxy <- sapply(seq_along(xy), function(i) {
						nm <- names(xy)[i]
						l <- coord(object)[[nm]] %in% xy[i]
						if ( all(!l) )
							.warning("coord out of range: ", nm, " = ", xy[i])
						l
					})
					if ( nrow(coord(object)) == 1 )
						lxy <- t(lxy)
					if ( is.null(dim(lxy)) ) {
						lxy <- which(lxy)
					} else {
						lxy <- which(apply(lxy, 1, all))
					}
				}))
				i1 <- as.integer(i1)
			} else {
				i1 <- seq_len(ncol(object))
			}
			if ( !missing(run) ) {
				if ( is.numeric(run) ) {
					i2 <- which(as.integer(run(object)) %in% run)
				} else {
					i2 <- which(run(object) %in% run)
				}
			} else {
				i2 <- seq_len(ncol(object))
			}
			pixels <- intersect(i1, i2)
			if ( length(match.call(expand.dots=FALSE)$...) > 0 ) {
				keep <- pixels %in% callNextMethod(object, ..., .env=.env)
				pixels <- pixels[keep]
			}
			names(pixels) <- pixelNames(object)[pixels]
		}
		pixels
	})

## rbind/cbind

setMethod("rbind", "MSImagingExperiment",
	function(..., deparse.level=1)
	{
		objects <- unname(list(...))
		is_centroided <- sapply(objects, "centroided")
		if ( !all(is_centroided) && !all(!is_centroided) )
			.warning("all object must be centroided or not centroided")
		result <- callNextMethod(..., deparse.level=deparse.level)
		result@centroided <- centroided(objects[[1]])
		result
    }
)

setMethod("cbind", "MSImagingExperiment",
	function(..., deparse.level=1)
	{
		objects <- unname(list(...))
		is_centroided <- sapply(objects, "centroided")
		if ( !all(is_centroided) && !all(!is_centroided) )
			.warning("all object must be centroided or not centroided")
		result <- callNextMethod(..., deparse.level=deparse.level)
		result@centroided <- centroided(objects[[1]])
		result
    }
)

## Pull data into memory

setMethod("collect", "MSImagingExperiment",
	function(x, ...)
	{
		x <- as(x, "MSImagingExperiment")
		data <- as(imageData(x), "SimpleList", strict=FALSE)
		imageData(x) <- as(endoapply(data, as.matrix), "MSContinuousImagingSpectraList")
		class(x) <- "MSContinuousImagingExperiment"
		if ( validObject(x) )
			x
	})

## coerce to/from MSImagingExperiment subclasses

setAs("MSImagingExperiment", "MSContinuousImagingExperiment",
	function(from) {
		if ( !is(imageData(from), "MSContinuousImagingSpectraList") )
			imageData(from) <- .to_MSContinuousImagingSpectraList(imageData(from))
		class(from) <- "MSContinuousImagingExperiment"
		from
	})

setAs("MSImagingExperiment", "MSProcessedImagingExperiment",
	function(from) {
		if ( !is(imageData(from), "MSProcessedImagingSpectraList") )
			imageData(from) <- .to_MSProcessedImagingSpectraList(imageData(from), mz(from))
		class(from) <- "MSProcessedImagingExperiment"
		from
	})

## coerce to/from MSImageSet

setAs("MSImageSet", "MSImagingExperiment",
	function(from) {
		fDataNames <- setdiff(names(fData(from)), "mz")
		pDataNames <- setdiff(names(pData(from)), c(coordLabels(from), "sample"))
		MSImagingExperiment(imageData=spectra(from),
			featureData=MassDataFrame(
				mz=mz(from),
				fData(from)[,fDataNames,drop=FALSE]),
			pixelData=PositionDataFrame(
				coord=DataFrame(coord(from)[,coordLabels(from)], row.names=NULL),
				run=pixelData(from)$sample,
				pData(from)[,pDataNames,drop=FALSE]),
			centroided=centroided(from))
	})

setAs("MSImagingExperiment", "MSImageSet",
	function(from) {
		out <- MSImageSet(spectra=spectra(from),
			mz=mz(from), coord=as.data.frame(coord(from)))
		pixelData(out)$sample <- run(from)
		fData <- as.data.frame(as(fData(from), "DataFrame"))
		pData <- as.data.frame(as(pData(from), "DataFrame"))
		fData(out) <- cbind(fData(out), fData)
		pData(out) <- cbind(pData(out), pData)
		centroided(out) <- centroided(out)
		out
	})

## show

setMethod("show", "MSImagingExperiment",
	function(object) {
		callNextMethod(object)
		# mass range
		mzr <- format(range(mz(object)))
		cat("    mass range:", mzr[1L], "to", mzr[2L], "\n")
		# centroided?
		cat("    centroided:", centroided(object), "\n")
	}
)


