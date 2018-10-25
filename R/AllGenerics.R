
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
setGeneric("coord", function(object) standardGeneric("coord"))
setGeneric("coord<-", function(object, value) standardGeneric("coord<-"))
setGeneric("coordLabels", function(object) standardGeneric("coordLabels"))
setGeneric("coordLabels<-", function(object, value) standardGeneric("coordLabels<-"))
setGeneric("features", function(object, ...) standardGeneric("features"))
setGeneric("modelData", function(object) standardGeneric("modelData"))
setGeneric("modelData<-", function(object, value) standardGeneric("modelData<-"))
setGeneric("mzData", function(object) standardGeneric("mzData"))
setGeneric("mzData<-", function(object, value) standardGeneric("mzData<-"))
setGeneric("peakData", function(object) standardGeneric("peakData"))
setGeneric("peakData<-", function(object, value) standardGeneric("peakData<-"))
setGeneric("pixels", function(object, ...) standardGeneric("pixels"))
setGeneric("pixelData", function(object) standardGeneric("pixelData"))
setGeneric("pixelData<-", function(object, value) standardGeneric("pixelData<-"))
setGeneric("pixelNames", function(object) standardGeneric("pixelNames"))
setGeneric("pixelNames<-", function(object, value) standardGeneric("pixelNames<-"))
setGeneric("positionArray", function(object) standardGeneric("positionArray"))
setGeneric("positionArray<-", function(object, value) standardGeneric("positionArray<-"))
setGeneric("processingData", function(object) standardGeneric("processingData")) # use MSnbase generic?
setGeneric("processingData<-", function(object, value) standardGeneric("processingData<-")) # use MSnbase generic?
setGeneric("dataCube", function(object, ...) standardGeneric("dataCube"))
setGeneric("regeneratePositions", function(object) standardGeneric("regeneratePositions"))
setGeneric("resolution", function(object) standardGeneric("resolution"))
setGeneric("resolution<-", function(object, value) standardGeneric("resolution<-"))
setGeneric("resultData", function(object) standardGeneric("resultData"))
setGeneric("resultData<-", function(object, value) standardGeneric("resultData<-"))
setGeneric("roiBind", function(object, ...) standardGeneric("roiBind"))
setGeneric("run", function(object) standardGeneric("run"))
setGeneric("run<-", function(object, value) standardGeneric("run<-"))
setGeneric("runNames", function(object) standardGeneric("runNames"))
setGeneric("runNames<-", function(object, value) standardGeneric("runNames<-"))

#### Supplement ProtGenerics  ####
## -------------------------------
setGeneric("peaks<-", function(object, ..., value) standardGeneric("peaks<-"))
setGeneric("spectra<-", function(object, ..., value) standardGeneric("spectra<-"))

#### MIAPE - Imaging ####
## ----------------------
setGeneric("msiInfo", function(object, ...) standardGeneric("msiInfo"))
setGeneric("specimenOrigin", function(object) standardGeneric("specimenOrigin"))
setGeneric("specimenOrigin<-", function(object, value) standardGeneric("specimenOrigin<-"))
setGeneric("specimenType", function(object) standardGeneric("specimenType"))
setGeneric("specimenType<-", function(object, value) standardGeneric("specimenType<-"))
setGeneric("stainingMethod", function(object) standardGeneric("stainingMethod"))
setGeneric("stainingMethod<-", function(object, value) standardGeneric("stainingMethod<-"))
setGeneric("tissueThickness", function(object) standardGeneric("tissueThickness"))
setGeneric("tissueThickness<-", function(object, value) standardGeneric("tissueThickness<-"))
setGeneric("tissueWash", function(object) standardGeneric("tissueWash"))
setGeneric("tissueWash<-", function(object, value) standardGeneric("tissueWash<-"))
setGeneric("embeddingMethod", function(object) standardGeneric("embeddingMethod"))
setGeneric("embeddingMethod<-", function(object, value) standardGeneric("embeddingMethod<-"))
setGeneric("inSituChemistry", function(object) standardGeneric("inSituChemistry"))
setGeneric("inSituChemistry<-", function(object, value) standardGeneric("inSituChemistry<-"))
setGeneric("matrixApplication", function(object) standardGeneric("matrixApplication"))
setGeneric("matrixApplication<-", function(object, value) standardGeneric("matrixApplication<-"))
setGeneric("pixelSize", function(object) standardGeneric("pixelSize"))
setGeneric("pixelSize<-", function(object, value) standardGeneric("pixelSize<-"))
setGeneric("instrumentModel", function(object) standardGeneric("instrumentModel")) # use MSnbase generic?
setGeneric("instrumentModel<-", function(object, value) standardGeneric("instrumentModel<-"))
setGeneric("instrumentVendor", function(object) standardGeneric("instrumentVendor"))
setGeneric("instrumentVendor<-", function(object, value) standardGeneric("instrumentVendor<-"))
setGeneric("massAnalyzerType", function(object) standardGeneric("massAnalyzerType"))
setGeneric("massAnalyzerType<-", function(object, value) standardGeneric("massAnalyzerType<-"))
setGeneric("ionizationType", function(object) standardGeneric("ionizationType"))
setGeneric("ionizationType<-", function(object, value) standardGeneric("ionizationType<-"))
setGeneric("scanPolarity", function(object) standardGeneric("scanPolarity"))
setGeneric("scanPolarity<-", function(object, value) standardGeneric("scanPolarity<-"))
setGeneric("softwareName", function(object) standardGeneric("softwareName")) # use MSnbase generic?
setGeneric("softwareName<-", function(object, value) standardGeneric("softwareName<-"))
setGeneric("softwareVersion", function(object) standardGeneric("softwareVersion")) # use MSnbase generic?
setGeneric("softwareVersion<-", function(object, value) standardGeneric("softwareVersion<-"))
setGeneric("scanType", function(object) standardGeneric("scanType"))
setGeneric("scanType<-", function(object, value) standardGeneric("scanType<-"))
setGeneric("scanPattern", function(object) standardGeneric("scanPattern"))
setGeneric("scanPattern<-", function(object, value) standardGeneric("scanPattern<-"))
setGeneric("scanDirection", function(object) standardGeneric("scanDirection"))
setGeneric("scanDirection<-", function(object, value) standardGeneric("scanDirection<-"))
setGeneric("lineScanDirection", function(object) standardGeneric("lineScanDirection"))
setGeneric("lineScanDirection<-", function(object, value) standardGeneric("lineScanDirection<-"))
setGeneric("imageShape", function(object) standardGeneric("imageShape"))
setGeneric("imageShape<-", function(object, value) standardGeneric("imageShape<-"))

#### Processing information ####
## -----------------------------
setGeneric("prochistory", function(object) standardGeneric("prochistory"))
setGeneric("prochistory<-", function(object, value) standardGeneric("prochistory<-"))
setGeneric("files", function(object) standardGeneric("files"))
setGeneric("files<-", function(object, value) standardGeneric("files<-"))
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
setGeneric("centroided", function(object) standardGeneric("centroided")) # use mzR generic?
setGeneric("centroided<-", function(object, value) standardGeneric("centroided<-"))

#### Apply-like methods ####
## ------------------------
setGeneric("cvApply", function(.x, .y, .fun, ...) standardGeneric("cvApply"))
setGeneric("featureApply", function(.object, .fun, ...) standardGeneric("featureApply"))
setGeneric("pixelApply", function(.object, .fun, ...) standardGeneric("pixelApply"))

#### Pre-processing ####
## ---------------------
setGeneric("process", function(object, ...) standardGeneric("process"))
setGeneric("batchProcess", function(object, ...) standardGeneric("batchProcess"))
setGeneric("smoothSignal", function(object, ...) standardGeneric("smoothSignal"))
setGeneric("peakPick", function(object, ...) standardGeneric("peakPick"))
setGeneric("peakAlign", function(object, ref, ...) standardGeneric("peakAlign"))
setGeneric("peakBin", function(object, ref, ...) standardGeneric("peakBin"))
setGeneric("peakFilter", function(object, ...) standardGeneric("peakFilter"))
setGeneric("reduceBaseline", function(object, ...) standardGeneric("reduceBaseline"))
setGeneric("reduceDimension", function(object, ref, ...) standardGeneric("reduceDimension"))
setGeneric("standardizeRuns", function(object, ...) standardGeneric("standardizeRuns"))

#### Data alignment ####
## ---------------------
setGeneric("coregister", function(object, ref, ...) standardGeneric("coregister"))

#### Data transformation ####
## --------------------------
setGeneric("arrange") # from 'dplyr'
setGeneric("filter") # from 'dplyr'
setGeneric("group_by") # from 'dplyr'
setGeneric("mutate") # from 'dplyr'
setGeneric("select") # from 'dplyr'
setGeneric("summarise") # from 'dplyr'
setGeneric("summarize")	# from 'dplyr'

#### Statistical analysis and tools ####
## -------------------------------------
setGeneric("OPLS", function(x, y, ...) standardGeneric("OPLS"))
setGeneric("PCA", function(x, ...) standardGeneric("PCA"))
setGeneric("PLS", function(x, y, ...) standardGeneric("PLS"))
setGeneric("spatialKMeans", function(x, ...) standardGeneric("spatialKMeans"))
setGeneric("spatialShrunkenCentroids", function(x, y, ...) standardGeneric("spatialShrunkenCentroids"))

#### Results ####
## --------------
setGeneric("topLabels", function(object, ...) standardGeneric("topLabels"))

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

