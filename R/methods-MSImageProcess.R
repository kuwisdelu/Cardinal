
setMethod("initialize", "MSImageProcess",
	function(.Object, ...) {
		.Object <- callNextMethod(.Object,...)
		.Object@CardinalVersion <- as.character(packageDescription("Cardinal", fields="Version"))
		.Object
	})

setMethod("show", "MSImageProcess",
	function(object) {
		cat("Processing data\n")
		cat("  Cardinal version:", object@CardinalVersion, "\n")
		cat("  Files:", paste(object@files, collapse="\n         "), "\n")
		cat("  Normalization:", object@normalization, "\n")
		cat("  Smoothing:", object@smoothing, "\n")
		cat("  Baseline reduction:", object@baselineReduction, "\n")
		cat("  Spectrum representation:", object@spectrumRepresentation, "\n")
		cat("  Peak picking:", object@peakPicking, "\n")
	})

setMethod("files", "MSImageProcess", function(object) object@files)
setReplaceMethod("files", "MSImageProcess",
	function(object, value) {
		object@files <- value
		object
	})

setMethod("normalization", "MSImageProcess", function(object) object@normalization)
setReplaceMethod("normalization", "MSImageProcess",
	function(object, value) {
		object@normalization <- value
		object
	})

setMethod("smoothing", "MSImageProcess", function(object) object@smoothing)
setReplaceMethod("smoothing", "MSImageProcess",
	function(object, value) {
		object@smoothing <- value
		object
	})

setMethod("baselineReduction", "MSImageProcess", function(object) object@baselineReduction)
setReplaceMethod("baselineReduction", "MSImageProcess",
	function(object, value) {
		object@baselineReduction <- value
		object
	})

setMethod("spectrumRepresentation", "MSImageProcess", function(object) object@spectrumRepresentation)
setReplaceMethod("spectrumRepresentation", "MSImageProcess",
	function(object, value) {
		object@spectrumRepresentation <- value
		object
	})

setMethod("peakPicking", "MSImageProcess", function(object) object@peakPicking)
setReplaceMethod("peakPicking", "MSImageProcess",
	function(object, value) {
		object@peakPicking <- value
		object
	})

setMethod("centroided", "MSImageProcess", function(object) object@centroided)
setReplaceMethod("centroided", "MSImageProcess",
	function(object, value) {
		object@centroided <- value
		object
	})

setMethod("prochistory", "MSImageProcess", function(object) object@history)

setReplaceMethod("prochistory", signature = c(object="MSImageProcess", value="list"),
	function(object, value) {
		object@history <- value
		object
	})

setReplaceMethod("prochistory", signature = c(object="MSImageProcess", value="character"),
	function(object, value) {
		object@history <- append(object@history, value)
		object
	})

## Adapted from combine(MIAME, MIAME) from Biobase
setMethod("combine",
	signature = c(x = "MSImageProcess", y = "MSImageProcess"),
	function(x, y, ...) {
		if ( identical(x,y) )
			return (x)
		for ( sl in names(getSlots(class(x))) ) {
			if ( identical(slot(x, sl),slot(y, sl)) )
				next
			slot(x, sl) <- switch(sl,
				## multiple elements possible
				files = ,
				normalization = ,
				smoothing = ,
				baselineReduction = ,
				spectrumRepresentation = ,
				peakPicking = ,
				centroided = ,
				history = ,
				CardinalVersion = {
					c(slot(x, sl), slot(y, sl))
				},
				.__classVersion__ = {
					.stop("'MSImageProcess' objects have different class version strings")
				},
				# unknown
				{
					warning("\n  unknown or conflicting information in MSImageProcess field '",
						sl,"'; using information from object ", x)
					slot(x, sl)
				})
		}
		if ( validObject(x) )
			return(x)
	})
