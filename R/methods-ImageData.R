
setMethod("initialize", "ImageData",
	function(.Object,
			data = new.env(parent=emptyenv()),
			storageMode = "immutableEnvironment",
			...) {
		.Object@data <- data
		dots <- match.call(expand.dots=FALSE)[["..."]]
		names <- names(dots)
		if ( length(dots) > 0 ) {
			if ( any(is.null(names)) ) stop("all elements must be named")
			obs <- list(...) # triggers copying of all objects in ...
			for ( nm in names )
				.Object@data[[nm]] <- obs[[nm]]
		}
		if ( storageMode %in% c("lockedEnvironment", "immutableEnvironment") )
			lockEnvironment(.Object@data, bindings=TRUE)
		.Object@storageMode <- storageMode
		callNextMethod(.Object)
	})

ImageData <- function(..., data = new.env(parent=emptyenv()),
	storageMode = c("immutableEnvironment",
		"lockedEnvironment", "environment"))
{
	storageMode <- match.arg(storageMode)
	dots <- match.call(expand.dots=FALSE)[["..."]]
	names <- names(dots)
	if ( any(is.null(names)) && length(dots) > 0 )
		stop("all elements must be named")
	.ImageData(..., data=data, storageMode=storageMode)
}

setMethod("iData", "ImageData", function(object) object@data)

setReplaceMethod("iData", "ImageData",
	function(object, value) {
		object@data <- value
		if ( storageMode(object) %in% c("lockedEnvironment", "immutableEnvironment"))
			lockEnvironment(object@data, bindings=TRUE)
		if ( validObject(object) )
			object
	})

## adapted from combine(AssayData, AssayData) from Biobase
setMethod("combine",
	signature = c(x = "ImageData", y = "ImageData"),
	function(x, y, ...) {
		storageMode <- storageMode(x)
		if ( storageMode(y) != storageMode)
			stop("ImageData must have same storage, but are '",
				storageMode, "', '", storageMode(y))
		if ( length(ls(x@data)) != length(ls(y@data)) )
			stop("ImageData have different numbers of elements:\n\t",
				paste(ls(x@data), collapse=" "), "\n\t",
				paste(ls(y@data), collapse=" "))
		if ( !all(ls(x@data) == ls(y@data)) )
			stop(paste("ImageData have different element names:",
				paste(ls(x@data), collapse=" "),
				paste(ls(y@data), collapse=" "), sep="\n\t"))
		data <- new.env(parent=emptyenv())
		for ( nm in ls(x@data) ) data[[nm]] <- combine(x[[nm]], y[[nm]])
		new("ImageData", data=data, storageMode=storageMode)
	})

setMethod("dims", "ImageData", function(object) {
	names <- ls(object@data)
	if ( length(names) > 0 ) {
		sapply(names, function(nm) dim(object@data[[nm]]))
	} else {
		matrix(nrow=0, ncol=0)
	}
})

setMethod("storageMode", "ImageData", function(object) object@storageMode)

setReplaceMethod("storageMode",
	signature = c(object = "ImageData", value = "character"),
	function(object, value) {
		if ( value == object@storageMode )
			return(object)
		names <- ls(object@data)
		data <- new.env(parent=emptyenv())
		for ( nm in names ) data[[nm]] <- object@data[[nm]]
		if ( value %in% c("lockedEnvironment", "immutableEnvironment"))
			lockEnvironment(data, bindings=TRUE)
		object@storageMode <- value
		object@data <- data
		if ( validObject(object) )
			object
	})

setMethod("annotatedDataFrameFrom", "ImageData",
	function(object, byrow, ...) {
		if ( nrow(dims(object)) == 0 ) {
			df <- IAnnotatedDataFrame()
		} else if ( is.null(rownames(dims(object))) ) {
			stop("ImageData must have named dimensions")
		} else if ( any(nchar(rownames(dims(object))) == 0) ) {
			stop("all dimensions of ImageData must have names")
		} else {
			data <- rep(list(integer()), nrow(dims(object)) - 1)
			names(data) <- rownames(dims(object))[-1]
			data <- as.data.frame(data)
			varMetadata <- data.frame(labelType=rep("dimension", nrow(dims(object)) - 1))
			df <- IAnnotatedDataFrame(data=data, varMetadata=varMetadata,
				dimLabels=c("pixelNames", "pixelColumns"))
		}
		df
	})

setMethod("[[", "ImageData", function(x, i, j, ..., value) x@data[[i]])

setReplaceMethod("[[", "ImageData", function(x, i, j, ..., value) {
	names <- ls(x@data, all.names=TRUE)
	if ( storageMode(x) == "immutableEnvironment" ) {
		data <- new.env(parent=emptyenv())
		for ( nm in names ) data[[nm]] <- x@data[[nm]]
		data[[i]] <- value
		lockEnvironment(data, bindings=TRUE)
	} else {
		data <- x@data
		data[[i]] <- value
	}
	x@data <- data
	x
})

