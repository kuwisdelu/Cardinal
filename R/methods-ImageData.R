
setMethod("initialize",
	signature(.Object = "ImageData"),
	function(.Object,
			data = new.env(parent=emptyenv()),
			storageMode = "immutableEnvironment",
			...) {
		obs <- list(...)
		if ( length(ls(.Object@data)) > 0 ) {
			names <- ls(.Object@data)
			for ( nm in names )
				data[[nm]] <- .Object@data[[nm]]
		}
		.Object@data <- data
		if ( length(obs) > 0 ) {
			names <- names(obs)
			if ( any(is.null(names)) ) stop("all elements must be named")
			for ( nm in names )
				.Object@data[[nm]] <- obs[[nm]]
		}
		if ( storageMode %in% c("lockedEnvironment", "immutableEnvironment") )
			lockEnvironment(.Object@data, bindings=TRUE)
		.Object@storageMode <- storageMode
		callNextMethod(.Object,
			data=data,
			storageMode=storageMode)
	})

ImageData <- function(..., storageMode = c("immutableEnvironment",
	"lockedEnvironment", "environment"))
{
	storageMode <- match.arg(storageMode)
	obs <- list(...)
	if ( any(is.null(names(obs))) && length(obs) > 0 )
		stop("all elements must be named")
	.ImageData(..., storageMode=storageMode)
}

setMethod("storageMode", signature = signature(object = "ImageData"), function(object) object@storageMode)

setReplaceMethod("storageMode",
	signature = signature(object = "ImageData", value = "character"),
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

setMethod("[[", "ImageData", function(x, i, j, ..., value) x@data[[i]])

setReplaceMethod("[[", "ImageData", function(x, i, j, ..., value) {
	names <- ls(x@data)
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

