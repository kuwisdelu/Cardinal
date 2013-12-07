
#### Pixel based AnnotatedDataFrame for imaging ####
## based on Biobase's AnnotatedDataFrame, but with 
## additions that reflect that each row belongs to a pixel
## and multiple rows will belong to the same sample
## ------------------------------------------------
.IAnnotatedDataFrame <- setClass("IAnnotatedDataFrame",
	contains = "AnnotatedDataFrame",
	prototype = prototype(new("Versioned",
		versions=c(IAnnotatedDataFrame="0.0.2"))))

#### 'Minimum Information About a Proteomics Experiment' - Imaging ####
## based on working MIAPE-Imaging document and imzML specification
## ---------------------------------------------------------------
setClass("MIAPE-Imaging",
	slots = c(
		## Experimental Info
		title = "character",
		abstract = "character",
		url = "character",
		pubMedIds = "character",
		preprocessing = "list", # pre-processing NOT performed by Cardinal
		other = "list",
		## Responsible Person
		name = "character",
		lab = "character",
		contact = "character",
		## Histomorphological Classification
		samples = "list", # sample ids
		specimenOrigin = "character", # institution
		specimenType = "character", # species, organ, ...
		stainingMethod = "character", # H&E, etc.
		## Tissue Preparation
		tissueThickness = "numeric",
		tissueWash = "character",
		embeddingMethod = "character",
		inSituChemistry = "character", # tryptic digest, ...
		matrixApplication = "character", # application method
		## Data Acquisition: Instrument Details
		pixelSize = "numeric",
		instrumentModel = "character",
		instrumentVendor = "character",
		massAnalyzerType = "character", # TOF, LTQ, ...
		ionizationType = "character", # MALDI, DESI, ...
		scanPolarity = "character",
		## Data Acquisition: Control Software
		softwareName = "character",
		softwareVersion = "character",
		## Data Acquisition: Scan Settings
		scanType = "character",
		scanPattern = "character",
		scanDirection = "character",
		lineScanDirection = "character",
		imageShape = "character"),
	contains = "MIAxE",
	prototype = prototype(
		new("Versioned", versions=c(classVersion("MIAxE"),
			`MIAPE-Imaging`="0.0.2"))))

#### Class for pre-processing information for an 'MSImageSet' ####
## should store all of the calls and their parameters
## --------------------------------------------------
setClass("MSImageProcess",
	slots = c(
		files = "character",
		normalization = "character",
		smoothing = "character",
		baselineReduction = "character",
		spectrumRepresentation = "character",
		peakPicking = "character",
		centroided = "logical",
		history = "list",
		CardinalVersion = "character"),
	contains = "Versioned",
	prototype = prototype(
		new("Versioned", versions=c(MSImageProcess="0.0.3")),
		CardinalVersion = character())) # set up in initialize

#### Class for generic imaging data ###
## simply holds an environment and a storage mode
## where elements of the environment are arrays
## --------------------------------------------
.ImageData <- setClass("ImageData",
	slots = c(
		data = "environment",
		storageMode = "character"),
	contains = "Versioned",
	prototype = prototype(
		new("Versioned", versions=c(ImageData="0.0.2")),
		data = new.env(parent=baseenv()), # re-assign in initialize
		storageMode = "immutableEnvironment"),
	validity = function(object) {
		msg <- validMsg(NULL, NULL)
		names <- ls(object@data)
		if ( !all(sapply(names, function(nm) !is.null(dim(object@data[[nm]])))) )
			msg <- validMsg(msg, "all elements must be an array-like object ('dims' of positive length)")
		ldim <- sapply(names, function(nm) length(dim(object@data[[nm]])))
		if ( !all(sapply(ldim, function(ld) ld == ldim[[1]])) )
			msg <- validMsg(msg, "all elements must have an equal number of dimensions")
		if ( !object@storageMode %in% c("environment", "lockedEnvironment", "immutableEnvironment") )
			msg <- validMsg(msg, "storageMode must be one of 'environment', 'lockedEnvironment', or 'immutableEnvironment'")
		if (is.null(msg)) TRUE else msg
	})

#### Class for holding sparse image data ####
## feature vectors are stored as a matrix and the datacube
## can be reconstructed as an array on-the-fly
## --------------------------------------------
.SImageData <- setClass("SImageData",
	slots = c(positionArray = "array"),
	contains = "ImageData",
	prototype = prototype(
		new("Versioned", versions=c(classVersion("ImageData"), SImageData="0.0.2")),
		positionArray = array(dim=c(x=0, y=0))),
	validity = function(object) {
		msg <- validMsg(NULL, NULL)
		names <- ls(object@data)
		if ( !all(sapply(names, function(nm) length(dim(object@data[[nm]])) == 2)) )
			msg <- validMsg(msg, "all data elements must be a matrix-like object ('dims' of length 2)")
		ncols <- sapply(names, function(nm) ncol(object@data[[nm]]))
		if ( !all(sapply(ncols, function(nc) nc == ncols[[1]])) )
			msg <- validMsg(msg, "all elements must have an equal number of columns")
		nrows <- sapply(names, function(nm) nrow(object@data[[nm]]))
		if ( !all(sapply(nrows, function(nr) nr == nrows[[1]])) )
			msg <- validMsg(msg, "all elements must have an equal number of rows")
		if ( sum(!is.na(object@positionArray)) > 0 && any(!is.integer(object@positionArray[!is.na(object@positionArray)])) )
			msg <- validMsg(msg, "positionArray must contain only integers and NAs")
		if ( any(sapply(names, function(nm) ncol(object@data[[nm]])) != sum(!is.na(object@positionArray))) )
			msg <- validMsg(msg, "number of non-NA indices in positionArray must match number of cols of data elements")
		if ( is.null(msg) ) TRUE else msg
	})

#### Matrix-like class for sparse signals ####
## implemented using lists as hash tables
## ---------------------------------------
.Hashmat <- setClass("Hashmat",
	slots = c(
		data = "list",
		keys = "character",
		dim = "numeric",
		dimnames = "list"),
	contains = "Versioned",
	prototype = prototype(
		new("Versioned", versions=c(Hashmat="0.0.1")),
		data = list(),
		keys = character(),
		dim = c(0, 0),
		dimnames = list(NULL, NULL)),
	validity = function(object) {
		msg <- validMsg(NULL, NULL)
		if ( any(duplicated(object@keys)) )
			msg <- validMsg(msg, "elements of keys must be unique")
		dm <- object@dim
		if ( dm[[1]] != length(object@keys) )
			msg <- validMsg(msg, paste("dims [", dm[[1]], "] does not match the length of keys [",
				length(object@data), "]", sep=""))
		if ( dm[[2]] != length(object@data) )
			msg <- validMsg(msg, paste("dims [", dm[[2]], "] does not match the length of data [",
				length(object@data), "]", sep=""))
		dmn <- object@dimnames
		if ( length(dmn) != 2 )
			msg <- validMsg(msg, paste("length of 'dimnames' [",
				length(dmn), "] must match that of 'dims' [2]", sep=""))
		if ( !is.null(dmn[[1]]) && length(dmn[[1]]) != dm[[1]] )
			msg <- validMsg(msg, paste("length of 'dimnames' [",
				length(dmn[[1]]), "] not equal to array extent", sep=""))
		if ( !is.null(dmn[[2]]) && length(dmn[[2]]) != dm[[2]] )
			msg <- validMsg(msg, paste("length of 'dimnames' [",
				length(dmn[[2]]), "] not equal to array extent", sep=""))
		if (is.null(msg)) TRUE else msg
	})

#### Class for generic imaging datasets ####
## heavily inspired by structure of Biobase's eSet
## ------------------------------------------------
setClass("iSet",
	slots = c(
		imageData = "ImageData", # holds an immutable environment
		pixelData = "IAnnotatedDataFrame", # analogous to phenoData
		featureData = "AnnotatedDataFrame",
		experimentData = "MIAxE",
		protocolData = "AnnotatedDataFrame"),
	contains = c("VersionedBiobase", "VIRTUAL"),
	prototype = prototype(
		new("VersionedBiobase", versions=c(iSet="0.0.1")),
		imageData = .ImageData(),
		pixelData = .IAnnotatedDataFrame(
			dimLabels = c("pixelNames", "pixelColumns")),
		featureData = new("AnnotatedDataFrame",
			dimLabels = c("featureNames", "featureColumns")),
		protocolData = new("AnnotatedDataFrame",
			dimLabels = c("sampleNames", "sampleColumns"))))

#### Class for pixel-sparse imaging datasets ####
## extends iSet with metadata and methods for MS imaging
## ----------------------------------------------------
.SImageSet <- setClass("SImageSet",
	slots = c(imageData = "SImageData"),
	contains = "iSet",
	prototype = prototype(
		new("VersionedBiobase", versions=c(classVersion("iSet"),
			SImageSet="0.0.1")),
		imageData = .SImageData()))

#### Class for mass spectrometry imaging datasets ####
## extends SImageSet with metadata for MS imaging
## ----------------------------------------------------
.MSImageSet <- setClass("MSImageSet",
	slots = c(
		processingData = "MSImageProcess",
		experimentData = "MIAPE-Imaging"),
	contains = "SImageSet",
	prototype = prototype(
		new("VersionedBiobase", versions=c(classVersion("iSet"),
			c(classVersion("SImageSet"), MSImageSet="0.5.2"))),
		imageData = .SImageData(),
		processingData = new("MSImageProcess"),
		experimentData = new("MIAPE-Imaging")))



