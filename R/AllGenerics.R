
#### Generics from stats ####
## --------------------------
setGeneric("predict")
setGeneric("fitted")

#### Basic getters and setters ####
## ---------------------------------
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
setGeneric("modelData", function(object, ...) standardGeneric("modelData"))
setGeneric("modelData<-", function(object, ..., value) standardGeneric("modelData<-"))

#### Generics from EBImage ####
## ----------------------------
setGeneric("imageData")
setGeneric("imageData<-")

#### Pre-processing ####
## ---------------------
setGeneric("process", function(object, ...) standardGeneric("process"))
setGeneric("recalibrate", function(object, ...) standardGeneric("recalibrate"))
setGeneric("reduceBaseline", function(object, ...) standardGeneric("reduceBaseline"))
setGeneric("peakPick", function(object, ...) standardGeneric("peakPick"))
setGeneric("peakAlign", function(object, ...) standardGeneric("peakAlign"))
setGeneric("peakProcess", function(object, ...) standardGeneric("peakProcess"))

#### Co-registration and co-localization ####
## ------------------------------------------
setGeneric("coregister", function(object, ref, ...) standardGeneric("coregister"))
setGeneric("colocalized", function(object, ref, ...) standardGeneric("colocalized"))

#### Data transformation ####
## --------------------------
setGeneric("slice", function(x, ...) standardGeneric("slice"))

#### Spatial utilities ####
## ---------------------
setGeneric("findNeighbors", function(x, ...) standardGeneric("findNeighbors"))
setGeneric("spatialWeights", function(x, ...) standardGeneric("spatialWeights"))

#### Statistical analysis and tools ####
## -------------------------------------
setGeneric("crossValidate", function(x, y, ...) standardGeneric("crossValidate"))
setGeneric("NMF", function(x, ...) standardGeneric("NMF"))
setGeneric("PCA", function(x, ...) standardGeneric("PCA"))
setGeneric("PLS", function(x, y, ...) standardGeneric("PLS"))
setGeneric("OPLS", function(x, y, ...) standardGeneric("OPLS"))
setGeneric("spatialFastmap", function(x, ...) standardGeneric("spatialFastmap"))
setGeneric("spatialKMeans", function(x, ...) standardGeneric("spatialKMeans"))
setGeneric("spatialShrunkenCentroids", function(x, y, ...) standardGeneric("spatialShrunkenCentroids"))
setGeneric("spatialDGMM", function(x, ...) standardGeneric("spatialDGMM"))
setGeneric("meansTest", function(x, ...) standardGeneric("meansTest"))
setGeneric("segmentationTest", function(x, ...) standardGeneric("segmentationTest"))

#### Results ####
## --------------
setGeneric("topFeatures", function(object, ...) standardGeneric("topFeatures"))

#### Plotting ####
## ---------------
setGeneric("selectROI", function(object, ...) standardGeneric("selectROI"))
setGeneric("image3D", function(x, ...) standardGeneric("image3D"))
setGeneric("is3D", function(object) standardGeneric("is3D"))

#### Deprecated ####
## ------------------

# getters and setters
setGeneric("iData", function(x, i, ...) standardGeneric("iData"))
setGeneric("iData<-", function(x, i, ..., value) standardGeneric("iData<-"))
setMethod("iData", "ANY", function(x, i, ...) {
		.Deprecated("spectra")
		spectra(x, i, ...)
	})
setReplaceMethod("iData", "ANY", function(x, i, ..., value) {
		.Deprecated("spectra")
		spectra(x, i, ...) <- values
		x
	})
setGeneric("coordLabels", function(object) standardGeneric("coordLabels"))
setGeneric("coordLabels<-", function(object, value) standardGeneric("coordLabels<-"))
setGeneric("peakData", function(object, ...) standardGeneric("peakData"))
setGeneric("peakData<-", function(object, ..., value) standardGeneric("peakData<-"))
setGeneric("mzData", function(object, ...) standardGeneric("mzData"))
setGeneric("mzData<-", function(object, ..., value) standardGeneric("mzData<-"))
setGeneric("intensityData", function(object, ...) standardGeneric("intensityData"))
setGeneric("intensityData<-", function(object, ..., value) standardGeneric("intensityData<-"))
setGeneric("resultData", function(object, ...) standardGeneric("resultData"))
setGeneric("resultData<-", function(object, ..., value) standardGeneric("resultData<-"))
setGeneric("resultNames", function(object, ...) standardGeneric("resultNames"))
setGeneric("resultNames<-", function(object, ..., value) standardGeneric("resultNames<-"))

# resolution
setGeneric("resolution", function(object) standardGeneric("resolution"))
setGeneric("resolution<-", function(object, value) standardGeneric("resolution<-"))

# 'sp' package
setGeneric("gridded", function(obj) standardGeneric("gridded"))
setGeneric("gridded<-", function(obj, value) standardGeneric("gridded<-"))
setGeneric("coordinates", function(obj, ...) standardGeneric("coordinates"))
setGeneric("coordinates<-", function(object, value) standardGeneric("coordinates<-"))
setGeneric("coordnames", function(x) standardGeneric("coordnames"))
setGeneric("coordnames<-", function(x, value) standardGeneric("coordnames<-"))

# imzML metadata
setGeneric("msiInfo", function(object, ...) standardGeneric("msiInfo"))
setGeneric("matrixApplication", function(object) standardGeneric("matrixApplication"))
setGeneric("pixelSize", function(object) standardGeneric("pixelSize"))
setGeneric("instrumentVendor", function(object) standardGeneric("instrumentVendor"))
setGeneric("massAnalyzerType", function(object) standardGeneric("massAnalyzerType"))
setGeneric("ionizationType", function(object) standardGeneric("ionizationType"))
setGeneric("scanPolarity", function(object) standardGeneric("scanPolarity"))
setGeneric("scanType", function(object) standardGeneric("scanType"))
setGeneric("scanPattern", function(object) standardGeneric("scanPattern"))
setGeneric("scanDirection", function(object) standardGeneric("scanDirection"))
setGeneric("lineScanDirection", function(object) standardGeneric("lineScanDirection"))

# preprocessing
setGeneric("normalization", function(object) standardGeneric("normalization"))
setGeneric("normalization<-", function(object, value) standardGeneric("normalization<-"))
setGeneric("smoothing", function(object) standardGeneric("smoothing"))
setGeneric("smoothing<-", function(object, value) standardGeneric("smoothing<-"))
setGeneric("baselineReduction", function(object) standardGeneric("baselineReduction"))
setGeneric("baselineReduction<-", function(object, value) standardGeneric("baselineReduction<-"))
setGeneric("spectrumRepresentation", function(object) standardGeneric("spectrumRepresentation"))
setGeneric("spectrumRepresentation<-", function(object, value) standardGeneric("spectrumRepresentation<-"))
setGeneric("peakPicking", function(object) standardGeneric("peakPicking"))
setGeneric("peakPicking<-", function(object, value) standardGeneric("peakPicking<-"))

setGeneric("smoothSignal", function(object, ...) standardGeneric("smoothSignal"))
setGeneric("mzAlign", function(object, ref, ...) standardGeneric("mzAlign"))
setGeneric("mzBin", function(object, ref, ...) standardGeneric("mzBin"))
setGeneric("mzFilter", function(object, ...) standardGeneric("mzFilter"))
setGeneric("peakBin", function(object, ref, ...) standardGeneric("peakBin"))
setGeneric("peakFilter", function(object, ...) standardGeneric("peakFilter"))

# apply
setGeneric("cvApply", function(.x, .y, .fun, ...) standardGeneric("cvApply"))
setGeneric("featureApply", function(.object, .fun, ...) standardGeneric("featureApply"))
setGeneric("pixelApply", function(.object, .fun, ...) standardGeneric("pixelApply"))
setGeneric("spatialApply", function(.object, .r, .fun, ...) standardGeneric("spatialApply"))

# images
setGeneric("height", function(x) standardGeneric("height"))
setGeneric("height<-", function(x, ..., value) standardGeneric("height<-"))

