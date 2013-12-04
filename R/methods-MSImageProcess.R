
setMethod("initialize", "MSImageProcess",
	function(.Object, ...) {
		.Object <- callNextMethod(.Object,...)
		.Object@CardinalVersion <- as.character(packageDescription("Cardinal", fields="Version"))
		.Object
	})

setMethod("show", "MSImageProcess",
	function(object) {
		for ( proc in object@history )
			cat(proc, "\n")
		cat("  Cardinal version:", object@CardinalVersion, "\n")
	})

setMethod("files", "MSImageProcess", function(object) object@files)

setMethod("normalization", "MSImageProcess", function(object) object@normalization)

setMethod("smoothing", "MSImageProcess", function(object) object@smoothing)

setMethod("baselineReduction", "MSImageProcess", function(object) object@baselineReduction)

setMethod("spectrumRepresentation", "MSImageProcess", function(object) object@spectrumRepresentation)

setMethod("peakPicking", "MSImageProcess", function(object) object@peakPicking)

setMethod("centroided", "MSImageProcess", function(object) object@centroided)

setMethod("exphistory", "MSImageProcess", function(object) object@history)

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
					stop("'MSImageProcess' objects have different class version strings")
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
