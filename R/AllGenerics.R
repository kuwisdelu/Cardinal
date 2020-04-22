
#### Define new generics from stats R ####
## ---------------------------------------
setGeneric("predict")
setGeneric("fitted")

#### Define new generics from EBImage ####
## ---------------------------------------
setGeneric("imageData")
setGeneric("imageData<-")

#### New accessor, setter, and manipulation ####
## -----------------------------------------------
setGeneric("iData", function(x, i, ...) standardGeneric("iData"))
setGeneric("iData<-", function(x, i, ..., value) standardGeneric("iData<-"))

#### Basic accessor, setter, and manipulation ####
## -----------------------------------------------
setGeneric("is3D", function(object) standardGeneric("is3D"))
setGeneric("coord", function(object) standardGeneric("coord"))
setGeneric("coord<-", function(object, value) standardGeneric("coord<-"))
setGeneric("coordLabels", function(object) standardGeneric("coordLabels"))
setGeneric("coordLabels<-", function(object, value) standardGeneric("coordLabels<-"))
setGeneric("run", function(object) standardGeneric("run"))
setGeneric("run<-", function(object, value) standardGeneric("run<-"))
setGeneric("runNames", function(object) standardGeneric("runNames"))
setGeneric("runNames<-", function(object, value) standardGeneric("runNames<-"))
setGeneric("peakData", function(object, ...) standardGeneric("peakData"))
setGeneric("peakData<-", function(object, ..., value) standardGeneric("peakData<-"))
setGeneric("mzData", function(object, ...) standardGeneric("mzData"))
setGeneric("mzData<-", function(object, ..., value) standardGeneric("mzData<-"))
setGeneric("intensityData", function(object, ...) standardGeneric("intensityData"))
setGeneric("intensityData<-", function(object, ..., value) standardGeneric("intensityData<-"))
setGeneric("features", function(object, ...) standardGeneric("features"))
setGeneric("pixels", function(object, ...) standardGeneric("pixels"))
setGeneric("pixelData", function(object) standardGeneric("pixelData"))
setGeneric("pixelData<-", function(object, value) standardGeneric("pixelData<-"))
setGeneric("pixelNames", function(object) standardGeneric("pixelNames"))
setGeneric("pixelNames<-", function(object, value) standardGeneric("pixelNames<-"))
setGeneric("resolution", function(object) standardGeneric("resolution"))
setGeneric("resolution<-", function(object, value) standardGeneric("resolution<-"))
setGeneric("modelData", function(object, ...) standardGeneric("modelData"))
setGeneric("modelData<-", function(object, ..., value) standardGeneric("modelData<-"))
setGeneric("resultData", function(object, i, ...) standardGeneric("resultData"))
setGeneric("resultData<-", function(object, i, ..., value) standardGeneric("resultData<-"))
setGeneric("resultNames", function(object, ...) standardGeneric("resultNames"))
setGeneric("resultNames<-", function(object, ..., value) standardGeneric("resultNames<-"))
setGeneric("showNames", function(object) standardGeneric("showNames"))

#### Legacy generics ####
## ----------------------
setGeneric("positionArray", function(object) standardGeneric("positionArray"))
setGeneric("positionArray<-", function(object, value) standardGeneric("positionArray<-"))
setGeneric("regeneratePositions", function(object) standardGeneric("regeneratePositions"))

#### MIAPE - Imaging ####
## ----------------------
setGeneric("msiInfo", function(object, ...) standardGeneric("msiInfo"))
setGeneric("specimenOrigin", function(object) standardGeneric("specimenOrigin"))
setGeneric("specimenType", function(object) standardGeneric("specimenType"))
setGeneric("stainingMethod", function(object) standardGeneric("stainingMethod"))
setGeneric("tissueThickness", function(object) standardGeneric("tissueThickness"))
setGeneric("tissueWash", function(object) standardGeneric("tissueWash"))
setGeneric("embeddingMethod", function(object) standardGeneric("embeddingMethod"))
setGeneric("inSituChemistry", function(object) standardGeneric("inSituChemistry"))
setGeneric("matrixApplication", function(object) standardGeneric("matrixApplication"))
setGeneric("pixelSize", function(object) standardGeneric("pixelSize"))
setGeneric("imageShape", function(object) standardGeneric("imageShape"))
setGeneric("instrumentVendor", function(object) standardGeneric("instrumentVendor"))
setGeneric("massAnalyzerType", function(object) standardGeneric("massAnalyzerType"))
setGeneric("ionizationType", function(object) standardGeneric("ionizationType"))
setGeneric("scanPolarity", function(object) standardGeneric("scanPolarity"))
setGeneric("softwareName", function(object) standardGeneric("softwareName")) # use MSnbase generic?
setGeneric("softwareVersion", function(object) standardGeneric("softwareVersion")) # use MSnbase generic?
setGeneric("scanType", function(object) standardGeneric("scanType"))
setGeneric("scanPattern", function(object) standardGeneric("scanPattern"))
setGeneric("scanDirection", function(object) standardGeneric("scanDirection"))
setGeneric("lineScanDirection", function(object) standardGeneric("lineScanDirection"))

#### Processing information ####
## -----------------------------
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

#### Apply-like methods ####
## ------------------------
setGeneric("crossValidate", function(.x, .y, .fun, ...) standardGeneric("crossValidate"))
setGeneric("cvApply", function(.x, .y, .fun, ...) standardGeneric("cvApply"))
setGeneric("featureApply", function(.object, .fun, ...) standardGeneric("featureApply"))
setGeneric("pixelApply", function(.object, .fun, ...) standardGeneric("pixelApply"))
setGeneric("spatialApply", function(.object, .r, .fun, ...) standardGeneric("spatialApply"))

#### Pre-processing ####
## ---------------------
setGeneric("process", function(object, ...) standardGeneric("process"))
setGeneric("batchProcess", function(object, ...) standardGeneric("batchProcess"))
setGeneric("smoothSignal", function(object, ...) standardGeneric("smoothSignal"))
setGeneric("reduceBaseline", function(object, ...) standardGeneric("reduceBaseline"))
setGeneric("mzAlign", function(object, ref, ...) standardGeneric("mzAlign"))
setGeneric("mzBin", function(object, ref, ...) standardGeneric("mzBin"))
setGeneric("mzFilter", function(object, ...) standardGeneric("mzFilter"))
setGeneric("peakPick", function(object, ...) standardGeneric("peakPick"))
setGeneric("peakAlign", function(object, ref, ...) standardGeneric("peakAlign"))
setGeneric("peakBin", function(object, ref, ...) standardGeneric("peakBin"))
setGeneric("peakFilter", function(object, ...) standardGeneric("peakFilter"))
setGeneric("reduceDimension", function(object, ref, ...) standardGeneric("reduceDimension"))
setGeneric("standardizeRuns", function(object, ...) standardGeneric("standardizeRuns"))

#### Data alignment and matching ####
## ----------------------------------
setGeneric("coregister", function(object, ref, ...) standardGeneric("coregister"))
setGeneric("colocalized", function(object, ref, ...) standardGeneric("colocalized"))

#### Image manipulation and metadata ####
## --------------------------------------
setGeneric("height", function(x) standardGeneric("height"))
setGeneric("height<-", function(x, ..., value) standardGeneric("height<-"))

#### Data transformation ####
## --------------------------
setGeneric("slice", function(x, ...) standardGeneric("slice"))

#### Spatial utilities ####
## ---------------------
setGeneric("findNeighbors", function(x, ...) standardGeneric("findNeighbors"))
setGeneric("spatialWeights", function(x, ...) standardGeneric("spatialWeights"))

#### Statistical analysis and tools ####
## -------------------------------------
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

#### Data export ####
## --------------
setGeneric("writeAnalyze", function(object, ...) standardGeneric("writeAnalyze"))
setGeneric("writeImzML", function(object, ...) standardGeneric("writeImzML"))
setGeneric("writeMSIData", function(object, file, ...) standardGeneric("writeMSIData"))

####-----------------------------------------------------------####

