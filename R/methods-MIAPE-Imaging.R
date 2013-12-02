
setMethod("imagingInfo", "MIAPE-Imaging", function(object) {
	cat("MIAPE-Imaging information:\n")
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

setMethod("specimenOrigin", "MIAPE-Imaging", function(object) object@specimenOrigin)
setMethod("specimenType", "MIAPE-Imaging", function(object) object@specimenType)
setMethod("stainingMethod", "MIAPE-Imaging", function(object) object@stainingMethod)

setMethod("tissueThickness", "MIAPE-Imaging", function(object) object@tissueThickness)
setMethod("tissueWash", "MIAPE-Imaging", function(object) object@tissueWash)
setMethod("embeddingMethod", "MIAPE-Imaging", function(object) object@embeddingMethod)
setMethod("inSituChemistry", "MIAPE-Imaging", function(object) object@inSituChemistry)
setMethod("matrixDeposition", "MIAPE-Imaging", function(object) object@matrixDeposition)

setMethod("pixelSize", "MIAPE-Imaging", function(object) object@pixelSize)
setMethod("lineScanDirection", "MIAPE-Imaging", function(object) object@lineScanDirection)
setMethod("scanDirection", "MIAPE-Imaging", function(object) object@scanDirection)
setMethod("scanPattern", "MIAPE-Imaging", function(object) object@scanPattern)
setMethod("scanType", "MIAPE-Imaging", function(object) object@scanType)
setMethod("imageShape", "MIAPE-Imaging", function(object) object@imageShape)

## adapted from combine(MIAPE, MIAPE) from MSnbase
setMethod("combine",
	signature = c(x = "MIAPE-Imaging", y = "MIAPE-Imaging"),
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
				## MIAPE-Imaging specific
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
					stop("'MIAPE-Imaging' objects have different class version strings")
				},
				## unknown
				{
					warning("\n  unknown or conflicting information in MIAPE-Imaging field '",
						sl,"'; using information from first object 'x'")
					slot(x, sl)
				})
		}
		if ( validObject(x) )
			return(x)
	})
