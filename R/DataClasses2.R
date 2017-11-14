
#### DataFrame with spatial information for imaging data ####
## --------------------------------------------------------
.PositionDataFrame <- setClass("PositionDataFrame",
	contains = "DataFrame",
	slots = c(
		coord = "DataFrame",
		gridded = "logical",
		resolution = "numeric"),
	prototype = prototype(
		coord = DataFrame(),
		resolution = c(NA_real_, NA_real_),
		rownames = NULL,
		nrows = 0L,
		listData = structure(list(), names = character())))

#### DataFrame with mass information for mass spectral data ####
## -------------------------------------------------------------
.MassDataFrame <- setClass("MassDataFrame",
	contains = "DataFrame",
	slots = c(
		mz = "numeric",
		resolution = "numeric"),
	prototype = prototype(
		mz = numeric(),
		resolution = NA_real_,
		rownames = NULL,
		nrows = 0L,
		listData = structure(list(), names = character())))

#### Class for a list of imaging data ###
## --------------------------------------
setClass("ImageList", contains = "VIRTUAL")

.SimpleImageList <- setRefClass("SimpleImageList",
    fields = list(data = "SimpleList"),
    contains = "ImageList")

.MSContinuousImagingSpectraList <- setRefClass(
	Class = "MSContinuousImagingSpectraList",
    contains = "SimpleImageList")

.MSProcessedImagingSpectraList <- setRefClass(
	Class = "MSProcessedImagingSpectraList",
    contains = "SimpleImageList")

setAs("SimpleList", "SimpleImageList",
    function(from) .SimpleImageList(data=from))

setAs("SimpleImageList", "SimpleList", function(from) from$data)

#### Class for generic imaging experiments ####
## ---------------------------------------------
.ImagingExperiment <- setClass("ImagingExperiment",
	contains = c("Vector", "VIRTUAL"),
	slots = c(
		imageData = "ImageList",
		featureData = "DataFrame",
		elementMetadata = "PositionDataFrame",
		processing = "SimpleList"),
	prototype = prototype(
		imageData = .SimpleImageList(),
		featureData = DataFrame(),
		elementMetadata = .PositionDataFrame(),
		metadata = list(),
		processing = SimpleList()))

#### Class for pixel-sparse imaging experiments ####
## --------------------------------------------------
.SparseImagingExperiment <- setClass("SparseImagingExperiment",
	contains = "ImagingExperiment",
	slots = c(imageData = "SimpleImageList"),
	prototype = prototype(
		imageData = .SimpleImageList()))

#### Class for mass spectrometry imaging experiments ####
## -------------------------------------------------------
.MSImagingExperiment <- setClass("MSImagingExperiment",
	contains = "SparseImagingExperiment",
	slots = c(
		featureData = "MassDataFrame",
		centroided = "logical"),
	prototype = prototype(
		imageData = .SimpleImageList(),
		featureData = .MassDataFrame(),
		elementMetadata = .PositionDataFrame(),
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

