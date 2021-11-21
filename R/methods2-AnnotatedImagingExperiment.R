
#### Methods for AnnotatedImagingExperiment ####
## ------------------------------------------

AnnotatedImagingExperiment <- function(imageData = AnnotatedImageList(),
	featureData = DataFrame(), phenoData = DataFrame(), metadata = list())
{
	if ( !is(imageData, "AnnotatedImageList") )
		imageData <- AnnotatedImageList(imageData)
	if ( length(imageData) != 0L ) {
		if ( missing(featureData) )
			featureData <- new("DFrame", nrows=nrow(imageData))
		if ( missing(phenoData) )
			phenoData <- new("DFrame", nrows=ncol(imageData),
				rownames=names(imageData))
	}
	.AnnotatedImagingExperiment(
		imageData=imageData,
		featureData=featureData,
		elementMetadata=phenoData,
		metadata=metadata)
}

.valid.AnnotatedImagingExperiment <- function(object) {
	errors <- NULL
	if ( length(object@imageData) != 0L ) {
		if ( nrow(object@imageData) != nrow(object@featureData) )
			errors <- c(errors , paste("number of rows in 'featureData'",
				"must match number of rows in 'imageData'"))
		if ( ncol(object@imageData) != nrow(object@elementMetadata) )
			errors <- c(errors , paste("number of rows in 'phenoData'",
				"must match number of columns in 'imageData'"))
	}
	if ( is.null(errors) ) TRUE else errors
}

setValidity("AnnotatedImagingExperiment", .valid.AnnotatedImagingExperiment)


setMethod("dims", "AnnotatedImagingExperiment",
	function(x) dims(imageData(x)))

# resolution is count of pixels per unit x/y axis step


setMethod("resolution", "AnnotatedImagingExperiment",
	function(object) {
		data <- as(imageData(object), "SimpleList", strict=FALSE)
		vapply(data, "resolution", numeric(1))
	})

# coord is offset x/y offset of top left corner

setMethod("coord", "AnnotatedImagingExperiment",
	function(object) {
		data <- as(imageData(object), "SimpleList", strict=FALSE)
		vapply(data, "coord", numeric(2))
	})


setMethod("coordinates", "AnnotatedImagingExperiment",
	function(obj, ..., simplify = TRUE) {
		data <- as(imageData(obj), "SimpleList", strict=FALSE)
		if ( simplify ) {
			vapply(data, "coord", numeric(1))
		} else {
			lapply(data, "coord")
		}
	})

# width calculated from resolution and count of x pixels

setMethod("width", "AnnotatedImagingExperiment",
	function(x) {
		data <- as(imageData(x), "SimpleList", strict=FALSE)
		vapply(data, "width", numeric(1))
	})


# height calculated from resolution and count of y pixels

setMethod("height", "AnnotatedImagingExperiment",
	function(x) {
		data <- as(imageData(x), "SimpleList", strict=FALSE)
		vapply(data, "height", numeric(1))
	})


## show

setMethod("show", "AnnotatedImagingExperiment",
	function(object) {
		# print parent information
		callNextMethod(object)
		# dims()
		t1 <- "    "
		images <- as(imageData(object), "SimpleList", strict=FALSE)
		dms <- sapply(images, function(x) {
			d <- paste0(dim(x), collapse=" x ")
			paste0("<", d, ">")
		})
		.scat("dims(%d): %s\n", dms, collapse=", ", prefix=t1)
		# colorMode()
		col <- sapply(images, function(x) {
			if ( colorMode(x) == EBImage::Color ) {
				"Color"
			} else {
				"Grayscale"
			}
		})
		.scat("colorMode(%d): %s", col, collapse=" ", prefix=t1)
	}
)


