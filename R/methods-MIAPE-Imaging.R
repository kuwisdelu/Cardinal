
## adapted from MIAME from Biobase

setMethod("show", "MIAPE-Imaging", function(object) {
	if ( length(object@instrumentModel) > 0 || length(object@instrumentVendor) > 0 ) {
		cat("Instrument : \n")
		cat("  Model:", object@instrumentModel,"\n")
		cat("  Vendor:", object@instrumentVendor,"\n")
		cat("  Use 'msiInfo(object)' for more MIAPE-Imaging information.\n")
	}
	tmp <- c("samples", "preprocessing")
	Index <- c(length(object@samples) > 0, length(object@preprocessing) > 0)
	cat("Experiment data\n")
	cat("  Experimenter name:", object@name,"\n")
	cat("  Laboratory:", object@lab,"\n")
	cat("  Contact:", object@contact,"\n")
	cat("  Title:", object@title,"\n")
	cat("  URL:", object@url,"\n")
	pmids <- pubMedIds(object)
	cat("  PMIDs:", pmids,"\n")
	if( length(object@abstract) > 0 && all(object@abstract!="") ) {
		cat("\n  Abstract: A", length(strsplit(object@abstract," ")[[1]]),
			"word abstract is available. Use 'abstract' method.\n")
	} else {
		cat("  No abstract available.\n")
	}
	if (any(Index))
	    cat("  Information is available on:", paste(tmp[Index],collapse=", "),"\n")
	nO = notes(object)
	if (length(nO) > 0) {
		cat("  notes:\n" )
		if( is.list(nO) ) {
			nms = names(nO)
			pw = options("width")[[1]] - 6
			for(i in 1:length(nO) ) {
				cat("   ", nms[i], ":", sep="")
				cat("     ", strbreak(nO[[i]], width=pw, exdent=0), sep="\n      ")
			}
		}
	}
})

## MIAPE-Imaging specific

setMethod("msiInfo", "MIAPE-Imaging", function(object) {
	cat("MIAPE-Imaging information:\n")
	cat("  Histomorphological information:\n")
	cat("    Specimen origin:", specimenOrigin(object),"\n")
	cat("    Specimen type:", specimenType(object),"\n")
	cat("    Staining method:", stainingMethod(object),"\n")
	cat("  Tissue preparation:\n")
	cat("    Tissue thickness:", tissueThickness(object)," um\n")
	cat("    Tissue wash:", tissueWash(object),"\n")
	cat("    Embedding method:", embeddingMethod(object),"\n")
	cat("    In-situ chemistry:", inSituChemistry(object),"\n")
	cat("    Matrix application:", matrixApplication(object),"\n")
	cat("  Data acquisition (Instrument):\n")
	cat("    Pixel size:", pixelSize(object)," um\n")
	cat("    Instrument model:", instrumentModel(object),"\n")
	cat("    Instrument vendor:", instrumentVendor(object),"\n")
	cat("    Mass analyzer type:", massAnalyzerType(object),"\n")
	cat("    Ionization type:", ionizationType(object),"\n")
	cat("    Scan polarity:", scanPolarity(object),"\n")
	cat("  Data acquisition (Software):\n")
	cat("    Software name:", softwareName(object),"\n")
	cat("    Software version:", softwareVersion(object),"\n")
	cat("  Data acquisition (Scan):\n")
	cat("    Scan type:", scanType(object),"\n")
	cat("    Scan pattern:", scanPattern(object),"\n")
	cat("    Scan direction:", scanDirection(object),"\n")
	cat("    Line scan direction:", lineScanDirection(object),"\n")
	cat("    Image shape:", imageShape(object),"\n")
})

## adapted from MIAME from Biobase

setMethod("abstract","MIAPE-Imaging",function(object) object@abstract)

setMethod("samples","MIAPE-Imaging",function(object) object@samples)

setMethod("preproc","MIAPE-Imaging",function(object) object@preprocessing)
setReplaceMethod("preproc", "MIAPE-Imaging", function(object, value) {
	object@preprocessing <- value
	object
})

setMethod("pubMedIds","MIAPE-Imaging",function(object) object@pubMedIds)
setReplaceMethod("pubMedIds","MIAPE-Imaging",function(object,value){
	object@pubMedIds = value
	object
})

setMethod("otherInfo","MIAPE-Imaging",function(object) object@other)

setMethod("expinfo","MIAPE-Imaging",
	function(object) {
		info <- c(object@name, object@lab, object@contact, object@title, object@url)
		names(info) <- c("name", "lab", "contact", "title", "url")
		return(info)
	})

setMethod("notes", signature(object="MIAPE-Imaging"), function(object) object@other)

setReplaceMethod("notes", signature = c(object="MIAPE-Imaging", value="list"),
	function(object, value) {
		object@other <- value
		object
	})

setReplaceMethod("notes", signature = c(object="MIAPE-Imaging", value="character"),
	function(object, value) {
		object@other <- append(object@other, value)
		object
	})

## specific to MIAPE-Imaging

setMethod("specimenOrigin", "MIAPE-Imaging", function(object) object@specimenOrigin)

setMethod("specimenType", "MIAPE-Imaging", function(object) object@specimenType)

setMethod("stainingMethod", "MIAPE-Imaging", function(object) object@stainingMethod)

setMethod("tissueThickness", "MIAPE-Imaging", function(object) object@tissueThickness)

setMethod("tissueWash", "MIAPE-Imaging", function(object) object@tissueWash)

setMethod("embeddingMethod", "MIAPE-Imaging", function(object) object@embeddingMethod)

setMethod("inSituChemistry", "MIAPE-Imaging", function(object) object@inSituChemistry)

setMethod("matrixApplication", "MIAPE-Imaging", function(object) object@matrixApplication)

setMethod("pixelSize", "MIAPE-Imaging", function(object) object@pixelSize)

setMethod("instrumentModel", "MIAPE-Imaging", function(object) object@instrumentModel)

setMethod("instrumentVendor", "MIAPE-Imaging", function(object) object@instrumentVendor)

setMethod("massAnalyzerType", "MIAPE-Imaging", function(object) object@massAnalyzerType)

setMethod("ionizationType", "MIAPE-Imaging", function(object) object@ionizationType)

setMethod("scanPolarity", "MIAPE-Imaging", function(object) object@scanPolarity)

setMethod("softwareName", "MIAPE-Imaging", function(object) object@softwareName)

setMethod("softwareVersion", "MIAPE-Imaging", function(object) object@softwareVersion)

setMethod("scanType", "MIAPE-Imaging", function(object) object@scanType)

setMethod("scanPattern", "MIAPE-Imaging", function(object) object@scanPattern)

setMethod("scanDirection", "MIAPE-Imaging", function(object) object@scanDirection)

setMethod("lineScanDirection", "MIAPE-Imaging", function(object) object@lineScanDirection)

setMethod("imageShape", "MIAPE-Imaging", function(object) object@imageShape)

## adapted from combine(MIAME, MIAME) from Biobase
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
				## Experimental Info
				title = ,
				url = ,
				pubMedIds = ,
				samples = ,
				preprocessing = ,
				other = ,
				## Responsible Person
				name = ,
				lab = ,
				contact = ,
				## Histomorphological Classification
				specimenOrigin = ,
				specimenType = ,
				stainingMethod = ,
				## Tissue Preparation
				tissueThickness = ,
				tissueWash = ,
				embeddingMethod = ,
				inSituChemistry = ,
				matrixApplication = ,
				## Data Acquisition: Instrument Details
				pixelSize = ,
				instrumentModel = ,
				instrumentVendor = ,
				massAnalyzerType = ,
				ionizationType = ,
				scanPolarity = ,
				## Data Acquisition: Control Software
				softwareName = ,
				softwareVersion = ,
				## Data Acquisition: Scan Settings
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
					.stop("'MIAPE-Imaging' objects have different class version strings")
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
