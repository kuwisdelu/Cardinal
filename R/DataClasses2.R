
#### Classes to overwrite 'show' method for DataFrame ###
## -----------------------------------------------------
setClass("XDataFrame", contains = "DataFrame")

#### DataFrame with position information for imaging data ####
## -----------------------------------------------------------
.PositionDataFrame <- setClass("PositionDataFrame",
	contains = "XDataFrame",
	slots = c(
		run = "factor",
		coord = "DataFrame",
		gridded = "logical",
		resolution = "numeric"),
	prototype = prototype(
		run = factor(),
		coord = DataFrame(),
		resolution = c(NA_real_, NA_real_),
		rownames = NULL,
		nrows = 0L,
		listData = structure(list(), names = character())))

#### DataFrame with mass information for mass spectral data ####
## -------------------------------------------------------------
.MassDataFrame <- setClass("MassDataFrame",
	contains = "XDataFrame",
	slots = c(
		mz = "numeric",
		resolution = "numeric"),
	prototype = prototype(
		mz = numeric(),
		resolution = NA_real_,
		rownames = NULL,
		nrows = 0L,
		listData = structure(list(), names = character())))

#### Virtual classes for a list of imaging data ###
## ------------------------------------------------
setClass("ImageList", contains = "VIRTUAL")

setClass("ImageArrayList", contains = c("ImageList", "VIRTUAL"))

#### Classes for a list of imaging data ###
## -----------------------------------------

.SimpleImageList <- setRefClass("SimpleImageList",
    fields = list(data = "SimpleList"),
    contains = "ImageList")

.SimpleImageArrayList <- setRefClass("SimpleImageArrayList",
    contains = c("SimpleImageList", "ImageArrayList"))

setAs("SimpleList", "SimpleImageList",
    function(from) .SimpleImageList(data=from))

setAs("SimpleList", "SimpleImageArrayList",
    function(from) .SimpleImageArrayList(data=from))

setAs("SimpleImageList", "SimpleList", function(from) from$data)

#### Classes for a list of MS imaging data ###
## -------------------------------------------

.MSContinuousImagingSpectraList <- setRefClass(
	Class = "MSContinuousImagingSpectraList",
    contains = "SimpleImageArrayList")

.MSProcessedImagingSpectraList <- setRefClass(
	Class = "MSProcessedImagingSpectraList",
    contains = "SimpleImageArrayList")

setAs("SimpleList", "MSContinuousImagingSpectraList",
    function(from) .MSContinuousImagingSpectraList(data=from))

setAs("SimpleList", "MSProcessedImagingSpectraList",
    function(from) .MSProcessedImagingSpectraList(data=from))

#### Class for generic imaging experiments ####
## ---------------------------------------------
.ImagingExperiment <- setClass("ImagingExperiment",
	contains = c("Vector", "VIRTUAL"),
	slots = c(
		imageData = "ImageList",
		featureData = "DataFrame",
		elementMetadata = "DataFrame"),
	prototype = prototype(
		imageData = .SimpleImageList(),
		elementMetadata = DataFrame()))

#### Class for pixel-sparse imaging experiments ####
## --------------------------------------------------
.SparseImagingExperiment <- setClass("SparseImagingExperiment",
	contains = "ImagingExperiment",
	slots = c(
		imageData = "ImageArrayList",
		featureData = "XDataFrame",
		elementMetadata = "PositionDataFrame",
		processing = "SimpleList"),
	prototype = prototype(
		imageData = .SimpleImageArrayList(),
		elementMetadata = .PositionDataFrame(),
		processing = SimpleList()))

#### Class for mass spectrometry imaging experiments ####
## -------------------------------------------------------
.MSImagingExperiment <- setClass("MSImagingExperiment",
	contains = "SparseImagingExperiment",
	slots = c(
		featureData = "MassDataFrame",
		centroided = "logical"),
	prototype = prototype(
		featureData = .MassDataFrame(),
		centroided = FALSE))

#### Class for 'continuous' mass spectrometry images ####
## -------------------------------------------------------
.MSContinuousImagingExperiment <- setClass("MSContinuousImagingExperiment",
	contains = "MSImagingExperiment",
	slots = c(imageData = "MSContinuousImagingSpectraList"),
	prototype = prototype(
		imageData = .MSContinuousImagingSpectraList()))

#### Class for 'processed' mass spectrometry images ####
## -----------------------------------------------------
.MSProcessedImagingExperiment <- setClass("MSProcessedImagingExperiment",
	contains = "MSImagingExperiment",
	slots = c(imageData = "MSProcessedImagingSpectraList"),
	prototype = prototype(
		imageData = .MSProcessedImagingSpectraList()))

#### Class for mass spectrometry imaging information
## --------------------------------------------------
.MSImagingInfo <- setClass("MSImagingInfo",
	contains = "Vector",
	slots = c(
		scanList = "DataTable",
		mzArrayList = "DataTable",
		intensityArrayList = "DataTable"),
	prototype = prototype(
		scanList = DataFrame(),
		mzArrayList = DataFrame(),
		intensityArrayList = DataFrame()))

