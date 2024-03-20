
#### VIRTUAL class for a DataFrame with key columns ####
## -----------------------------------------------------
setClass("XDataFrame",
	contains = c("VIRTUAL", "DataFrame"),
	slots = c(keys = "list"))

setClass("XDFrame", contains = c("XDataFrame", "DFrame"))

#### DataFrame with position information ####
## ------------------------------------------
setClass("PositionDataFrame", contains = "XDFrame")

#### DataFrame with mass-to-charge information ####
## ------------------------------------------------
setClass("MassDataFrame", contains = "XDFrame")

#### List of spectra arrays with array-like subsetting ####
## ---------------------------------------------------------

setClass("SpectraArrays",
	contains = "Vector",
    slots = c(data = "SimpleList"))

#### VIRTUAL class for spectra-based imaging data ####
## ---------------------------------------------------
setClass("SpectralImagingData",
	contains = c("VIRTUAL", "Vector"),
	slots = c(
		spectraData = "SpectraArrays",
		elementMetadata = "PositionDataFrame",
		processing = "list"))

#### Class for spectra-based imaging arrays ####
## ----------------------------------------------
setClassUnion("ImzMeta_OR_NULL", c("ImzMeta", "NULL"))

setClass("SpectralImagingArrays",
	contains = "SpectralImagingData")

setClass("MSImagingArrays",
	contains = "SpectralImagingArrays",
	slots = c(
		experimentData = "ImzMeta_OR_NULL",
		centroided = "logical",
		continuous = "logical"))

#### Class for spectra-based imaging experiments ####
## --------------------------------------------------
setClass("SpectralImagingExperiment",
	contains = c("RectangularData", "SpectralImagingData"),
	slots = c(featureData = "DataFrame"))

setClass("MSImagingExperiment",
	contains = "SpectralImagingExperiment",
	slots = c(
		featureData = "MassDataFrame",
		experimentData = "ImzMeta_OR_NULL",
		centroided = "logical"))

#### Class union for MS-based imaging experiments ####
## ----------------------------------------------------
setClassUnion("MSImagingExperiment_OR_Arrays",
	c("MSImagingExperiment", "MSImagingArrays"))

#### Class for spatially-aware analysis results ####
## -------------------------------------------------

setClass("ResultsList", contains = "SimpleList")

setClass("SpatialResultsList",
	contains = "ResultsList",
	slots = c(
		featureData = "DataFrame",
		pixelData = "PositionDataFrame"))

