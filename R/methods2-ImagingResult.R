
#### Methods for ImagingResult ####
## ------------------------------------------

.valid.ImagingResult <- function(object) {
	errors <- NULL
	if ( length(object@resultData) != nrow(object@modelData) )
		errors <- c(errors , paste("length of 'resultData'",
			"must match number of rows in 'modelData'"))
	if ( is.null(errors) ) TRUE else errors
}

setValidity("ImagingResult", .valid.ImagingResult)

setMethod("resultData", c("ImagingResult", "missing"), 
	function(object, ...) object@resultData)

setMethod("resultData", c("ImagingResult", "ANY"), 
	function(object, i, j, ...) {
		if ( is.list(i) )
			i <- subset_rows(modelData(object), i)
		if ( missing(j) ) {
			object@resultData[[i, exact=FALSE]]
		} else {
			object@resultData[[i, exact=FALSE]][[j, exact=FALSE]]
		}
	})

setReplaceMethod("resultData", c("ImagingResult", "missing"), 
	function(object, ..., value) {
		object@resultData <- value
		if ( validObject(object) )
			object
	})

setReplaceMethod("resultData", c("ImagingResult", "ANY"), 
	function(object, i, j, ..., value) {
		if ( is.list(i) )
			i <- subset_rows(modelData(object), i)
		if ( missing(j) ) {
			object@resultData[[i, exact=TRUE]] <- value
		} else {
			object@resultData[[i, exact=TRUE]][[j, exact=TRUE]] <- value
		}
		if ( validObject(object) )
			object
	})

setMethod("resultNames", "ImagingResult", 
	function(object, i, ...) {
		if ( !missing(i) && is.list(i) )
			i <- subset_rows(modelData(object), i)
		if ( missing(i) ) {
			lapply(resultData(object), names)
		} else {
			names(resultData(object, i))
		}
	})

setMethod("modelData", "ImagingResult",
	function(object, ...) object@modelData)

setReplaceMethod("modelData", "ImagingResult",
	function(object, ..., value) {
		object@modelData <- value
		if ( validObject(object) )
			object			
	})

setMethod("[[", c("ImagingResult", "ANY", "ANY"),
	function(x, i, j, ...) {
		if ( !missing(j) ) {
			x@resultData[[i]][[j]]
		} else {
			x@resultData[[i]]
		}
	})

setReplaceMethod("[[", c("ImagingResult", "ANY", "ANY"),
	function(x, i, j, ..., value) {
		if ( !missing(j) ) {
			x@resultData[[i]][[j]] <- value
		} else {
			x@resultData[[i]] <- value
		}
	})

setMethod("[[", c("SparseImagingResult", "ANY", "ANY"),
	function(x, i, j, ...) {
		if ( !missing(j) ) {
			x@resultData[[i]][[j]]
		} else {
			x@resultData[[i]]
		}
	})

setReplaceMethod("[[", c("SparseImagingResult", "ANY", "ANY"),
	function(x, i, j, ..., value) {
		if ( !missing(j) ) {
			x@resultData[[i]][[j]] <- value
		} else {
			x@resultData[[i]] <- value
		}
	})

.DollarNames.ImagingResult <- function(x, pattern = "")
	grep(pattern, names(resultData(x, 1L)), value=TRUE)

setMethod("$", "ImagingResult",
	function(x, name) {
		lapply(x@resultData, function(res) res[[name, exact=FALSE]])
	})

## subsetting

setMethod("[", "SparseImagingResult",
	function(x, i, j, ..., drop) {
		lst <- (nargs() - !missing(drop)) < 3L
		if ( lst ) {
			x@resultData <- x@resultData[i]
			x@modelData <- x@modelData[i,,drop=FALSE]
			return(x)
		}
		if ( !missing(i) && (is.character(i) || is.factor(i)) )
			i <- match(i, featureNames(x))
		if ( !missing(j) && (is.character(j) || is.factor(j)) )
			j <- match(j, pixelNames(x))
		results <- x@resultData
		models <- x@modelData
		x <- callNextMethod(x, i=i, j=j, ..., drop=drop)
		kind <- metadata(x)$mapping
		if ( !is.null(kind) ) {
			results <- endoapply(results, function(res, i, j) {
				if ( !missing(i) )
					for ( name in kind$feature )
						res[[name]] <- res[[name]][i,,drop=FALSE]
				if ( !missing(j) )
					for ( name in kind$pixel )
						res[[name]] <- res[[name]][j,,drop=FALSE]
				res
			}, i=i, j=j)
		}
		x@resultData <- results
		x@modelData <- models
		x
	})

## combine

setMethod("combine", "SparseImagingResult",
	function(x, y, ...) {
		x@resultData <- c(x@resultData, y@resultData)
		x@modelData <- rbind(x@modelData, y@modelData)
		if ( validObject(x) )
			x
	}
)

setMethod("rbind", "SparseImagingResult",
	function(..., deparse.level=1) .stop("can't rbind results"))

setMethod("cbind", "SparseImagingResult",
	function(..., deparse.level=1) .stop("can't cbind results"))


# show

.show.ImagingResult <- function(object) {
	t1 <- "    "
	# resultData()
	if ( !is.null(resultNames(object, 1L)) )
		.scat("resultNames(%d): %s\n", resultNames(object, 1L), prefix=t1)
	resultDataNames <- names(resultData(object))
	if ( is.null(resultDataNames) )
		resultDataNames <- character(length(resultData(object)))
	.scat("resultData(%d): %s\n", resultDataNames, prefix=t1)
	# modelData()
	.scat("modelData(%d): %s\n", names(modelData(object)), prefix=t1)
}

setMethod("show", "ImagingResult",
	function(object) {
		callNextMethod(object)
		.show.ImagingResult(object)
	}
)

setMethod("show", "SparseImagingResult",
	function(object) {
		.show_SparseIE <- selectMethod("show", "SparseImagingExperiment")
		.show_SparseIE(object)
		.show.ImagingResult(object)
	}
)

# coerce from ResultSet

.coerce_ImagingResult <- function(from, toclass) {
	new(toclass,
		imageData=.SimpleImageArrayList(),
		featureData=XDataFrame(fData(from)),
		elementMetadata=PositionDataFrame(
			coord=DataFrame(coord(from)[,coordLabels(from)],
				row.names=NULL),
			run=pixelData(from)$sample),
		resultData=as(resultData(from), "List"),
		modelData=DataFrame(pData(modelData(from))))
}

