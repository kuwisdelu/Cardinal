
#### Methods for AnnotatedImage ####
## -----------------------------------

AnnotatedImage <- function(data = array(0, dim=c(1,1)),
							dim, colormode, ...)
{
	.AnnotatedImage(Image(data, dim, colormode), ...)
}

.valid.AnnotatedImage <- function(object) {
	errors <- NULL
	if ( length(object@offset) != 2L )
		errors <- c(errors , "'offset' must be length 2")
	if ( length(object@resolution) != 1L )
		errors <- c(errors , "'resolution' must be length 1")
	if ( is.null(errors) ) TRUE else errors
}

setValidity("AnnotatedImage", .valid.AnnotatedImage)

setAs("Image", "AnnotatedImage", function(from) .AnnotatedImage(from))

# length is product of first 2 dimensions

setMethod("length", "AnnotatedImage",
	function(x) prod(dim(x)[c(1,2)]))

# resolution is count of pixels per unit x/y axis step

setMethod("resolution", "AnnotatedImage",
	function(object) object@resolution)

setReplaceMethod("resolution", "AnnotatedImage",
	function(object, value) {
		object@resolution <- value
		if ( validObject(object) )
			object
	})

# coord is offset x/y offset of top left corner

setMethod("coord", "AnnotatedImage",
	function(object) object@offset)

setReplaceMethod("coord", "AnnotatedImage",
	function(object, value) {
		object@offset <- value
		if ( validObject(object) )
			object
	})

setMethod("coordinates", "AnnotatedImage",
	function(obj, ...) obj@offset)

setReplaceMethod("coordinates", "AnnotatedImage",
	function(object, value) {
		object@offset <- value
		if ( validObject(object) )
			object
	})

# width calculated from resolution and count of x pixels

setMethod("width", "AnnotatedImage",
	function(x) dim(x)[1L] / resolution(x))

setReplaceMethod("width", "AnnotatedImage",
	function(x, value) {
		x@resolution <- dim(x)[1L] / value
		if ( validObject(x) )
			x
	})

# height calculated from resolution and count of y pixels

setMethod("height", "AnnotatedImage",
	function(x) dim(x)[2L] / resolution(x))

setReplaceMethod("height", "AnnotatedImage",
	function(x, value) {
		x@resolution <- dim(x)[2L] / value
		if ( validObject(x) )
			x
	})

# misc methods

setMethod("range", "AnnotatedImage",
	function(x, ..., na.rm = FALSE) {
		y <- imageData(x)
		ymin <- min(y, na.rm=na.rm)
		ymax <- max(y, na.rm=na.rm)
		c(ymin, ymax) # range() takes weirdly long
	})




