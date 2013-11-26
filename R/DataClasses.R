
#### Pixel based AnnotatedDataFrame for imaging ####
## based on Biobase's AnnotatedDataFrame, but with 
## additions that reflect that each row belongs to a pixel
## and multiple rows will belong to the same sample
## ------------------------------------------------
.IAnnotatedDataFrame <- setClass("IAnnotatedDataFrame",
	contains = "AnnotatedDataFrame",
	prototype = prototype(
		new("Versioned", versions=c(IAnnotatedDataFrame="0.0.2"))))

#### Class for generic imaging data ###
## simply holds an environment and a storage mode
## --------------------------------------------
.ImageData <- setClass("ImageData",
	representation(
		data = "environment",
		storageMode = "character"),
	contains = "Versioned",
	prototype = prototype(
		new("Versioned", versions=c(ImageData="0.0.2")),
		data = new.env(parent=emptyenv()),
		storageMode = "immutableEnvironment"),
	validity = function(object) {
		msg <- validMsg(NULL, NULL)
		names <- ls(object@data)
		if ( !all(sapply(names, function(nm) is.array(object@data[[nm]]))) )
			msg <- validMsg(msg, "all elements must be an array")
		dims <- lapply(names, function(nm) dim(object@data[[nm]]))
		if ( !all(sapply(dims, function(dm) all(dm == dims[[1]]))) )
			msg <- ValidMsg(msg, "all elements must have the same dimensions")
		if ( !object@storageMode %in% c("environment", "lockedEnvironment", "immutableEnvironment") )
			msg <- validMsg(msg, "storageMode must be one of 'environment', 'lockedEnvironment', or 'immutableEnvironment'")
		if (is.null(msg)) TRUE else msg
	})

#### Class for holding imaging mass spectra ####
## spectra are stored as a matrix and the datacube
## can be reconstructed as an array on-the-fly
## --------------------------------------------
.MSImageSpectra <- setClass("MSImageSpectra",
	representation(positionArray = "array"),
	contains = "ImageData",
	prototype = prototype(
		new("Versioned", versions=c(MSImageSpectra="0.0.1")),
		positionArray = array()),
	validity = function(object) {
		msg <- validMsg(NULL, NULL)
		names <- ls(object@data)
		if ( !all(sapply(names, function(nm) is.matrix(object@data[[nm]]))) )
			msg <- validMsg("all data elements must be a matrix")
		if ( sum(!is.na(object@positionArray)) > 0 && any(!is.integer(object@positionArray[!is.na(object@positionArray)])) )
			msg <- validMsg(msg, "positionArray must contain only integers and NAs")
		if ( any(sapply(names, function(nm) ncol(object@data[[nm]])) != sum(!is.na(object@positionArray))) )
			msg <- validMsg(msg, "number of non-NA indices in positionArray must match number of cols of data elements")
		if ( is.null(msg) ) TRUE else msg
	})

#### Class for generic imaging datasets ####
## heavily inspired by structure of Biobase's eSet
## ------------------------------------------------
.iSet <- setClass("iSet",
	representation(
		imageData = "ImageData", # holds an immutable environment
		pixelData = "IAnnotatedDataFrame", # analogous to phenoData
		featureData = "AnnotatedDataFrame",
		experimentData = "MIAxE",
		protocolData = "AnnotatedDataFrame",
		"VIRTUAL"),
	contains = "VersionedBiobase",
	prototype = prototype(
		new("VersionedBiobase", versions=c(iSet="0.0.1")),
			imageData = .ImageData(),
			pixelData = .IAnnotatedDataFrame(
				dimLabels = c("pixelNames", "pixelColumns")),
			featureData = new("AnnotatedDataFrame",
				dimLabels = c("featureNames", "featureColumns")),
			protocolData = new("AnnotatedDataFrame",
				dimLabels = c("sampleNames", "sampleColumns"))))


