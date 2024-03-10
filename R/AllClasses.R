
#### Basic accessor, setter, and manipulation ####
## -----------------------------------------------

setGeneric("keys", function(object, ...) standardGeneric("keys"))
setGeneric("keys<-", function(object, ..., value) standardGeneric("keys<-"))
setGeneric("features", function(object, ...) standardGeneric("features"))
setGeneric("pixels", function(object, ...) standardGeneric("pixels"))
setGeneric("pixelData", function(object) standardGeneric("pixelData"))
setGeneric("pixelData<-", function(object, value) standardGeneric("pixelData<-"))
setGeneric("pixelNames", function(object) standardGeneric("pixelNames"))
setGeneric("pixelNames<-", function(object, value) standardGeneric("pixelNames<-"))
setGeneric("coord", function(object, ...) standardGeneric("coord"))
setGeneric("coord<-", function(object, ..., value) standardGeneric("coord<-"))
setGeneric("coordNames", function(object) standardGeneric("coordNames"))
setGeneric("coordNames<-", function(object, value) standardGeneric("coordNames<-"))
setGeneric("run", function(object, ...) standardGeneric("run"))
setGeneric("run<-", function(object, ..., value) standardGeneric("run<-"))
setGeneric("runNames", function(object) standardGeneric("runNames"))
setGeneric("runNames<-", function(object, value) standardGeneric("runNames<-"))

#### Pre-processing ####
## ---------------------
setGeneric("process", function(object, ...) standardGeneric("process"))
setGeneric("reduceBaseline", function(object, ...) standardGeneric("reduceBaseline"))
setGeneric("peakPick", function(object, ref, ...) standardGeneric("peakPick"))
setGeneric("peakAlign", function(object, ref, ...) standardGeneric("peakAlign"))
setGeneric("peakFilter", function(object, ...) standardGeneric("peakFilter"))

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

#### Class for analysis results ####
## ---------------------------------

# setClass("ResultsList", contains = "SimpleList")

