
#### Pixel based AnnotatedDataFrame for imaging ####
## based on Biobase's AnnotatedDataFrame, but with 
## additions that reflect that each row belongs to a pixel
## and multiple rows will belong to the same sample
## ------------------------------------------------
.IAnnotatedDataFrame <- setClass("IAnnotatedDataFrame",
	contains = "AnnotatedDataFrame",
	prototype = prototype(
		new("Versioned", versions=c(IAnnotatedDataFrame="0.0.2"))))

#### 'Minimum Information About a Proteomics Experiment' - Imaging ####
## extends MIAPE from the MSnbase package with imaging information 
## ---------------------------------------------------------------
setClass("MIAPE-Imaging",
	slots = c(
		## 1. Histomorphological classification
		specimenOrigin = "character",
		specimenType = "character",
		stainingMethod = "character",
		## 2. Tissue preparation
		tissueThickness = "numeric",
		tissueWash = "character",
		embeddingMethod = "character",
		inSituChemistry = "character",
		matrixDeposition = "character",
		## 3. Scan settings
		pixelSize = "numeric",
		scanType = "character",
		scanPattern = "character",
		scanDirection = "character",
		lineScanDirection = "character",
		imageShape = "character"),
	contains = "MIAPE",
	prototype = prototype(
		new("Versioned", versions=c(classVersion("MIAxE"),
			classVersion("MIAPE"), `MIAPE-Imaging`="0.0.1"))))

#### Class for pre-processing information for an 'MSImageSet' ####
## should store all of the calls and their parameters
## --------------------------------------------------
setClass("MSImageProcess",
	slots = c(
		removedBaseline = "logical",
		binned = "logical",
		resampled = "logical",
		centroided = "logical",
		history = "list",
		CardinalVersion = "character"),
	contains = "MSnProcess",
	prototype = prototype(
		new("Versioned", versions=c(classVersion("MSnProcess"),
			MSImageProcess="0.0.2")),
		history = new.env(parent=emptyenv()), # re-assign in initialize
		CardinalVersion = character())) # set up in initialize

#### Class for generic imaging data ###
## simply holds an environment and a storage mode
## --------------------------------------------
.ImageData <- setClass("ImageData",
	slots = c(
		data = "environment",
		storageMode = "character"),
	contains = "Versioned",
	prototype = prototype(
		new("Versioned", versions=c(ImageData="0.0.2")),
		data = new.env(parent=emptyenv()), # re-assign in initialize
		storageMode = "immutableEnvironment"),
	validity = function(object) {
		msg <- validMsg(NULL, NULL)
		names <- ls(object@data)
		if ( !all(sapply(names, function(nm) is.array(object@data[[nm]]))) )
			msg <- validMsg(msg, "all elements must be an array")
		ldim <- sapply(names, function(nm) length(dim(object@data[[nm]])))
		if ( !all(sapply(ldim, function(ld) ld == ldim[[1]])) )
			msg <- validMsg(msg, "all elements must have an equal number of dimensions")
		if ( !object@storageMode %in% c("environment", "lockedEnvironment", "immutableEnvironment") )
			msg <- validMsg(msg, "storageMode must be one of 'environment', 'lockedEnvironment', or 'immutableEnvironment'")
		if (is.null(msg)) TRUE else msg
	})

#### Class for holding imaging mass spectra ####
## spectra are stored as a matrix and the datacube
## can be reconstructed as an array on-the-fly
## --------------------------------------------
.MSImageSpectra <- setClass("MSImageSpectra",
	slots = c(positionArray = "array"),
	contains = "ImageData",
	prototype = prototype(
		new("Versioned", versions=c(MSImageSpectra="0.0.1")),
		positionArray = array()),
	validity = function(object) {
		msg <- validMsg(NULL, NULL)
		names <- ls(object@data)
		if ( !all(sapply(names, function(nm) is.matrix(object@data[[nm]]))) )
			msg <- validMsg(msg, "all data elements must be a matrix")
		ncols <- sapply(names, function(nm) ncol(object@data[[nm]]))
		if ( !all(sapply(ncols, function(nc) nc == ncols[[1]])) )
			msg <- validMsg(msg, "all elements must have an equal number of columns")
		if ( sum(!is.na(object@positionArray)) > 0 && any(!is.integer(object@positionArray[!is.na(object@positionArray)])) )
			msg <- validMsg(msg, "positionArray must contain only integers and NAs")
		if ( any(sapply(names, function(nm) ncol(object@data[[nm]])) != sum(!is.na(object@positionArray))) )
			msg <- validMsg(msg, "number of non-NA indices in positionArray must match number of cols of data elements")
		if ( is.null(msg) ) TRUE else msg
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

#### Class for mass spectrometry imaging datasets ####
## extends iSet with metadata and methods for MS imaging
## ----------------------------------------------------

.MSImageSet <- setClass("MSImageSet",
	slots = c(
		imageData = "MSImageSpectra",
		processingData = "MSImageProcess",
		experimentData = "MIAPE-Imaging"),
	contains = "iSet",
	prototype = prototype(
		new("VersionedBiobase", versions=c(classVersion("iSet"),
			MSImageSet="0.5.1")),
		imageData = .MSImageSpectra(),
		processingData = new("MSImageProcess"),
		experimentData = new("MIAPE-Imaging")))



