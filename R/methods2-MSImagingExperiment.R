
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
		if ( !centroided(object) )
			.stop("cannot select 'peaks'; object is not centroided")
		iData(object, ...)
	})

setReplaceMethod("peaks", "MSImagingExperiment",
	function(object, ..., value) {
		if ( !centroided(object) )
			.stop("cannot assign 'peaks'; object is not centroided")
		iData(object, ...) <- value
		object
	})

# 'continuous' imaging experiments

setReplaceMethod("iData", c("MSContinuousImagingExperiment", "missing"),
	function(x, i, ..., value) {
		if ( !inherits(value, c("matrix", "matter_matc")) ) {
			x <- as(x, "MSImagingExperiment")
			imageData(x) <- .SimpleImageArrayList(imageData(x))
		}
		callNextMethod(x, ..., value=value)
	})

setReplaceMethod("iData", c("MSContinuousImagingExperiment", "ANY"),
	function(x, i, ..., value) {
		if ( !inherits(value, c("matrix", "matter_matc")) ) {
			x <- as(x, "MSImagingExperiment")
			imageData(x) <- .SimpleImageArrayList(imageData(x))
		}
		callNextMethod(x, i=i, ..., value=value)
	})

# 'processed' imaging experiments

setReplaceMethod("iData", c("MSProcessedImagingExperiment", "missing"),
	function(x, i, ..., value) {
		if ( !inherits(value, "sparse_matc") ) {
			x <- as(x, "MSImagingExperiment")
			imageData(x) <- .SimpleImageArrayList(imageData(x))
		}
		callNextMethod(x, ..., value=value)
	})

setReplaceMethod("iData", c("MSProcessedImagingExperiment", "ANY"),
	function(x, i, ..., value) {
		if ( !inherits(value, "sparse_matc") ) {
			x <- as(x, "MSImagingExperiment")
			imageData(x) <- .SimpleImageArrayList(imageData(x))
		}
		callNextMethod(x, i=i, ..., value=value)
	})

setMethod("mzData", "MSProcessedImagingExperiment",
	function(object) atomdata(iData(object))[["keys"]])

setMethod("peakData", "MSProcessedImagingExperiment",
	function(object) atomdata(iData(object))[["values"]])

setMethod("mz", "missing",
	function(from, to, by = 400, units = c("ppm", "mz"), ...) {
		units <- match.arg(units)
		halfwidth <- by / 2
		mz <- switch(units,
			ppm = seq.ppm(from=from, to=to, ppm=halfwidth),
			mz = seq(from=from, to=to, by=halfwidth))
		tol <- switch(units,
			ppm = c(relative = halfwidth * 1e-6),
			mz = c(absolute = halfwidth))
		attr(mz, "tolerance") <- tol
		mz
	})

setReplaceMethod("mz", "MSProcessedImagingExperiment",
	function(object, value) {
		if ( length(value) != length(mz(object)) ) {
			if ( ncol(featureData(object)) > 0L ) {
				mcols <- names(featureData(object))
				.warning("dropping feature metadata cols: ", mcols)
			}
			object@featureData <- MassDataFrame(mz=value)
		}
		keys(object@imageData) <- value
		if ( !is.null(attr(value, "tolerance")) )
			tolerance(object@imageData) <- attr(value, "tolerance")
		if ( !is.null(attr(value, "combiner")) )
			combiner(object@imageData) <- attr(value, "combiner")
		if ( validObject(object) )
			object
	})

setMethod("tolerance", "MSProcessedImagingExperiment",
	function(object) tolerance(imageData(object)))

setReplaceMethod("tolerance", "MSProcessedImagingExperiment",
	function(object, value) {
		tolerance(imageData(object)) <- value
		object
	})

setMethod("combiner", "MSProcessedImagingExperiment",
	function(object) combiner(imageData(object)))

setReplaceMethod("combiner", "MSProcessedImagingExperiment",
	function(object, value) {
		combiner(imageData(object)) <- value
		object
	})

## Filter pixels/features

setMethod("features", "MSImagingExperiment",
	function(object, ..., mz, withNames = TRUE) {
		if ( missing(mz) ) {
			features <- callNextMethod(object, ..., withNames=withNames)
		} else {
			mz <- as.numeric(mz)
			features <- bsearch(mz, mz(object), nearest=TRUE)
			if ( length(list(...)) > 0 ) {
				keep <- features %in% callNextMethod(object, ..., withNames=FALSE)
				features <- features[keep]
			}
			if ( withNames )
				names(features) <- featureNames(object)[features]
		}
		features
	})

setMethod("pixels", "MSImagingExperiment",
	function(object, ..., coord, withNames = TRUE) {
		if ( missing(coord) ) {
			pixels <- callNextMethod(object, ..., withNames=withNames)
		} else {
			if ( !gridded(object) )
				.warning("pixel coordinates are not gridded")
			coord <- as.data.frame(as.list(coord))
			pixels <- unlist(apply(coord, 1, function(xy) {
				lxy <- sapply(seq_along(xy), function(i) {
					nm <- names(xy)[i]
					coord(object)[[nm]] %in% xy[i]
				})
				if ( nrow(coord(object)) == 1 )
					lxy <- t(lxy)
				if ( is.null(dim(lxy)) ) {
					lxy <- which(lxy)
				} else {
					lxy <- which(apply(lxy, 1, all))
				}
			}))
			if ( length(list(...)) > 0 ) {
				keep <- pixels %in% callNextMethod(object, ..., withNames=FALSE)
				pixels <- pixels[keep]
			}
			if ( withNames )
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

## coerce to/from MSImagingExperiment subclasses

setAs("MSImagingExperiment", "MSContinuousImagingExperiment",
	function(from) {
		if ( !is(imageData(from), "MSContinuousImagingSpectraList") )
			imageData(from) <- .MSContinuousImagingSpectraList(imageData(from))
		class(from) <- "MSContinuousImagingExperiment"
		from
	})

setAs("MSImagingExperiment", "MSProcessedImagingExperiment",
	function(from) {
		if ( !is(imageData(from), "MSProcessedImagingSpectraList") )
			imageData(from) <- .MSProcessedImagingSpectraList(imageData(from))
		class(from) <- "MSProcessedImagingExperiment"
		from
	})

## coerce to/from MSImageSet

setAs("MSImageSet", "MSImagingExperiment",
	function(from) {
		fDataNames <- setdiff(names(fData(from)), "mz")
		pDataNames <- setdiff(names(pData(from)), names(coord(from)))
		MSImagingExperiment(imageData=spectra(from),
			featureData=MassDataFrame(
				mz=mz(from),
				fData(from)[,fDataNames,drop=FALSE]),
			pixelData=PositionDataFrame(
				coord=DataFrame(coord(from), row.names=NULL),
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
