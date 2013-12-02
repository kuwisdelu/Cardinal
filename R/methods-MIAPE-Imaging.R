
setMethod("imagingInfo", "ImagingMIAPE", function(object) {
	cat("ImagingMIAPE information:\n")
	cat(" 1. Histomorphological information:\n")
	cat("  Specimen origin:", specimenOrigin(object),"\n")
	cat("  Specimen type:", specimenType(object),"\n")
	cat("  Staining method:", stainingMethod(object),"\n")
	cat(" 2. Tissue preparation\n")
	cat("  Tissue thickness:", tissueThickness(object)," um\n")
	cat("  Tissue wash:", tissueWash(object),"\n")
	cat("  Embedding method:", embeddingMethod(object),"\n")
	cat("  In-situ chemistry:", inSituChemistry(object),"\n")
	cat("  Matrix deposition:", matrixDeposition(object),"\n")
	cat(" 3. Scan settings\n")
	cat("  Pixel size:", pixelSize(object)," um\n")
	cat("  Scan type:", scanType(object),"\n")
	cat("  Scan pattern:", scanPattern(object),"\n")
	cat("  Scan direction:", scanDirection(object),"\n")
	cat("  Line scan direction:", lineScanDirection(object),"\n")
	cat("  Image shape:", imageShape(object),"\n")
})

setMethod("specimenOrigin", "ImagingMIAPE", function(object) object@specimenOrigin)
setMethod("specimenType", "ImagingMIAPE", function(object) object@specimenType)
setMethod("stainingMethod", "ImagingMIAPE", function(object) object@stainingMethod)

setMethod("tissueThickness", "ImagingMIAPE", function(object) object@tissueThickness)
setMethod("tissueWash", "ImagingMIAPE", function(object) object@tissueWash)
setMethod("embeddingMethod", "ImagingMIAPE", function(object) object@embeddingMethod)
setMethod("inSituChemistry", "ImagingMIAPE", function(object) object@inSituChemistry)
setMethod("matrixDeposition", "ImagingMIAPE", function(object) object@matrixDeposition)

setMethod("pixelSize", "ImagingMIAPE", function(object) object@pixelSize)
setMethod("lineScanDirection", "ImagingMIAPE", function(object) object@lineScanDirection)
setMethod("scanDirection", "ImagingMIAPE", function(object) object@scanDirection)
setMethod("scanPattern", "ImagingMIAPE", function(object) object@scanPattern)
setMethod("scanType", "ImagingMIAPE", function(object) object@scanType)
setMethod("imageShape", "ImagingMIAPE", function(object) object@imageShape)

## adapted from combine(MIAPE, MIAPE) from MSnbase
setMethod("combine",
	signature = c(x = "ImagingMIAPE", y = "ImagingMIAPE"),
	function(x, y, ...) {
		if ( identical(x,y) )
			return (x)
		for ( sl in names(getSlots(class(x))) ) {
			if ( identical(slot(x, sl),slot(y, sl)) )
				next
			slot(x, sl) <- switch(sl,
				## multiple elements possible
				## shared slots with MIAME
				name = ,
				lab = ,
				contact = ,
				title = ,
				url = ,
				pubMedIds = ,
				samples = ,
				hybridizations = ,
				normControls = ,
				preprocessing = ,
				other = ,
				## MIAPE specific
				dataStamp = ,
				instrumentModel = ,
				instrumentManufacturer = ,
				instrumentCustomisations = ,
				softwareName = ,
				softwareVersion = ,
				switchingCriteria = ,
				isolationWidth = ,
				parameterFile = ,
				ionSource = ,
				ionSourceDetails = ,
				analyser = ,
				analyserDetails = ,
				collisionGas = ,
				collisionPressure = ,
				collisionEnergy = ,
				detectorType = ,
				detectorSensitivity = ,
				## ImagingMIAPE specific
				specimenOrigin = ,
				specimenType = ,
				stainingMethod = ,
				tissueThickness = ,
				tissueWash = ,
				embeddingMethod = ,
				inSituChemistry = ,
				matrixDeposition = ,
				pixelSize = ,
				scanType = ,
				scanPattern = ,
				scanDirection = ,
				lineScanDirection = ,
				imageShape = {
					c(slot(x, sl), slot(y, sl))
				},
				## just a single entry
				abstract = {
					paste(slot(x, sl), slot(y, sl), collapse = "\n")
				},
				.__classVersion__ = {
					stop("'ImagingMIAPE' objects have different class version strings")
				},
				## unknown
				{
					warning("\n  unknown or conflicting information in ImagingMIAPE field '",
						sl,"'; using information from first object 'x'")
					slot(x, sl)
				})
		}
		if ( validObject(x) )
			return(x)
	})
