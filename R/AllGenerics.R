
#### Define new generics from base R ####
## -------------------------------------
setGeneric("as.array")
setGeneric("as.matrix")
setGeneric("length")
setGeneric("names")
setGeneric("names<-")
setGeneric("rownames<-")
setGeneric("colnames<-")
setGeneric("summary")

#### Define new generics from stats R ####
## ---------------------------------------
setGeneric("predict")
setGeneric("fitted")
setGeneric("residuals")
setGeneric("coef")

#### Define new generics from graphics R ####
## ------------------------------------------
setGeneric("plot")
setGeneric("image")

#### Basic accessor, setter, and manipulation ####
## -----------------------------------------------
setGeneric("coord", function(object) standardGeneric("coord"))
setGeneric("coord<-", function(object, value) standardGeneric("coord<-"))
setGeneric("coordinates", function(object) coord(object))
setGeneric("coordinates<-", function(object, value) coord(object, value))
setGeneric("coordLabels", function(object) standardGeneric("coordLabels"))
setGeneric("coordLabels<-", function(object, value) standardGeneric("coordLabels<-"))
setGeneric("features", function(object, ...) standardGeneric("features"))
setGeneric("iData", function(object) standardGeneric("iData"))
setGeneric("iData<-", function(object, value) standardGeneric("iData<-"))
setGeneric("imageData", function(object) standardGeneric("imageData"))
setGeneric("imageData<-", function(object, value) standardGeneric("imageData<-"))
setGeneric("intensities", function(object, ...) standardGeneric("intensities"))
setGeneric("keys", function(object) standardGeneric("keys"))
setGeneric("keys<-", function(object, value) standardGeneric("keys<-"))
setGeneric("modelData", function(object) standardGeneric("modelData"))
setGeneric("modelData<-", function(object, value) standardGeneric("modelData<-"))
setGeneric("mz", function(object) standardGeneric("mz")) # use MSnbase generic?
setGeneric("mz<-", function(object, value) standardGeneric("mz<-"))
setGeneric("mzData", function(object) standardGeneric("mzData"))
setGeneric("mzData<-", function(object, value) standardGeneric("mzData<-"))
setGeneric("peaks", function(object) standardGeneric("peaks")) # use mzR generic?
setGeneric("peaks<-", function(object, value) standardGeneric("peaks<-"))
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
setGeneric("regeneratePositions", function(object) standardGeneric("regeneratePositions"))
setGeneric("resultData", function(object) standardGeneric("resultData"))
setGeneric("resultData<-", function(object, value) standardGeneric("resultData<-"))
setGeneric("roiBind", function(object, ...) standardGeneric("roiBind"))
setGeneric("spectra", function(object) standardGeneric("spectra")) # use MSnbase generic?
setGeneric("spectra<-", function(object, value) standardGeneric("spectra<-"))

#### MIAPE - Imaging ####
## ----------------------
setGeneric("msiInfo", function(object) standardGeneric("msiInfo"))
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
setGeneric("smoothSignal", function(object, ...) standardGeneric("smoothSignal"))
setGeneric("peakPick", function(object, ...) standardGeneric("peakPick"))
setGeneric("peakAlign", function(object, ref, ...) standardGeneric("peakAlign"))
setGeneric("peakFilter", function(object, ...) standardGeneric("peakFilter"))
setGeneric("reduceBaseline", function(object, ...) standardGeneric("reduceBaseline"))
setGeneric("reduceDimension", function(object, ref, ...) standardGeneric("reduceDimension"))
setGeneric("standardizeSamples", function(object, ...) standardGeneric("standardizeSamples"))

#### Data transformation and alignment ####
## ---------------------------------------
# setGeneric("as2D", function(object) standardGeneric("as2D"))
# setGeneric("as3D", function(object) standardGeneric("as3D"))
setGeneric("coregister", function(object, ref, ...) standardGeneric("coregister"))

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
setGeneric("select", function(x, ...) standardGeneric("select"))
# setGeneric("image3d", function(x, ...) standardGeneric("image3d"))

####-----------------------------------------------------------####

