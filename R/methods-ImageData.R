
setMethod("initialize", "ImageData",
	function(.Object,
			data = new.env(parent=emptyenv()),
			storageMode = "immutableEnvironment",
			...) {
		.Object@data <- data
		dots <- match.call(expand.dots=FALSE)$...
		names <- names(dots)
		if ( length(dots) > 0 ) {
			if ( any(is.null(names)) ) .stop("all elements must be named")
			for ( nm in names ) {
				tryCatch({
					.Object@data[[nm]] <- eval(dots[[nm]])
				}, error=function(e) {
					.Object@data[[nm]] <- list(...)[[nm]]
				})
			}
		}
		if ( storageMode == "lockedEnvironment" )
			lockEnvironment(.Object@data, bindings=TRUE)
		.Object@storageMode <- storageMode
		callNextMethod(.Object)
	})

ImageData <- function(...,
	data = new.env(parent=emptyenv()),
	storageMode = c("immutableEnvironment",
		"lockedEnvironment",
		"environment"))
{
	storageMode <- match.arg(storageMode)
	dots <- match.call(expand.dots=FALSE)$...
	names <- names(dots)
	if ( any(is.null(names)) && length(dots) > 0 )
		.stop("all elements must be named")
	.ImageData(..., data=data, storageMode=storageMode)
}

setValidity("ImageData", function(object) {
	msg <- validMsg(NULL, NULL)
	names <- ls(object@data)
	if ( !all(sapply(names, function(nm) !is.null(dim(object@data[[nm]])))) )
		msg <- validMsg(msg, "all elements must be an array-like object ('dim' of positive length)")
	ldim <- sapply(names, function(nm) length(dim(object@data[[nm]])))
	if ( !all(sapply(ldim, function(ld) ld == ldim[1])) )
		msg <- validMsg(msg, "all elements must have an equal number of dimensions")
	if ( !object@storageMode %in% c("environment", "lockedEnvironment", "immutableEnvironment") )
		msg <- validMsg(msg, "storageMode must be one of 'environment', 'lockedEnvironment', or 'immutableEnvironment'")
	if (is.null(msg)) TRUE else msg
})

setMethod("names", "ImageData", function(x) ls(x@data, all.names=TRUE))

setReplaceMethod("names", "ImageData", function(x, value) {
	names <- ls(x@data, all.names=TRUE)
	if ( length(names) != length(value) ) {
		.stop(paste("'names' attribute [", length(value), "] must be",
			"the same length as the vector [", length(names), "]", sep=""))
	}
	if ( storageMode(x) == "immutableEnvironment" ) {
		data <- new.env(parent=emptyenv())
	} else {
		data <- x@data
	}
	for ( i in seq_along(names) ) data[[value[[i]]]] <- x@data[[names[[i]]]]
	if ( storageMode(x) != "immutableEnvironment" ) {
		rm(list=setdiff(names, value), envir=data)
	}
	x@data <- data
	x
})

setMethod("dims", "ImageData", function(object) {
	names <- ls(object@data)
	if ( length(names) > 0 ) {
		sapply(names, function(nm) dim(object@data[nm]))
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
		if ( value == "lockedEnvironment" )
			lockEnvironment(data, bindings=TRUE)
		object@storageMode <- value
		object@data <- data
		object
	})

setMethod("annotatedDataFrameFrom", "ImageData",
	function(object, byrow, ...) {
		if ( nrow(dims(object)) == 0 ) {
			df <- IAnnotatedDataFrame()
		} else if ( is.null(rownames(dims(object))) ) {
			.stop("ImageData must have named dimensions")
		} else if ( any(nchar(rownames(dims(object))) == 0) ) {
			.stop("all dimensions of ImageData must have names")
		} else {
			data <- rep(list(integer()), nrow(dims(object)) - 1)
			names(data) <- rownames(dims(object))[-1]
			data <- as.data.frame(data)
			varMetadata <- data.frame(labelType=rep("dim", nrow(dims(object)) - 1))
			df <- IAnnotatedDataFrame(data=data, varMetadata=varMetadata,
				dimLabels=c("pixelNames", "pixelColumns"))
		}
		df
	})

setMethod("[[", signature(x="ImageData", i="character", j="missing"),
	function(x, i, j, ..., value) x@data[[i]])

setReplaceMethod("[[", signature(x="ImageData", i="character", j="missing"),
	function(x, i, j, ..., value)
	{
		names <- ls(x@data, all.names=TRUE)
		if ( storageMode(x) == "immutableEnvironment" ) {
			data <- new.env(parent=emptyenv())
			for ( nm in names ) data[[nm]] <- x@data[[nm]]
			data[[i]] <- value
		} else {
			data <- x@data
			data[[i]] <- value
		}
		x@data <- data
		x
	})

## Adapted from combine(AssayData, AssayData) from Biobase
setMethod("combine",
	signature = c(x = "ImageData", y = "ImageData"),
	function(x, y, ...) {
		storageMode <- storageMode(x)
		if ( storageMode(y) != storageMode)
			.stop("ImageData must have same storage, but are '",
				storageMode, "', '", storageMode(y))
		if ( length(ls(x@data)) != length(ls(y@data)) )
			.stop("ImageData have different numbers of elements:\n\t",
				paste(ls(x@data), collapse=" "), "\n\t",
				paste(ls(y@data), collapse=" "))
		if ( !all(ls(x@data) == ls(y@data)) )
			.stop(paste("ImageData have different element names:",
				paste(ls(x@data), collapse=" "),
				paste(ls(y@data), collapse=" "), sep="\n\t"))
		data <- new.env(parent=emptyenv())
		for ( nm in ls(x@data) ) data[[nm]] <- combine(x[[nm]], y[[nm]])
		new(class(x), data=data, storageMode=storageMode)
	})

setMethod("show", "ImageData", function(object) {
	cat("An object of class '", class(object), "'\n", sep="")
	for ( nm in ls(object@data) ) {
		ob <- object@data[[nm]]
		dms <- paste0(dim(ob), collapse=" x ")
		size <- format(object.size(ob), units="Mb")
		cat("  ", nm, ": ", dms, " ", class(ob), " (", size, ")\n", sep="")
	}
})

