
#### Define new generics from stats R ####
## ---------------------------------------
setGeneric("predict")
setGeneric("fitted")

#### Define new generics from EBImage ####
## ---------------------------------------
setGeneric("imageData")
setGeneric("imageData<-")

#### Basic getters and setters ####
## --------------------------------
setGeneric("is3D", function(object) standardGeneric("is3D"))
setGeneric("coord", function(object) standardGeneric("coord"))
setGeneric("coord<-", function(object, value) standardGeneric("coord<-"))
setGeneric("run", function(object) standardGeneric("run"))
setGeneric("run<-", function(object, value) standardGeneric("run<-"))
setGeneric("runNames", function(object) standardGeneric("runNames"))
setGeneric("runNames<-", function(object, value) standardGeneric("runNames<-"))
setGeneric("features", function(object, ...) standardGeneric("features"))
setGeneric("pixels", function(object, ...) standardGeneric("pixels"))
setGeneric("pixelData", function(object) standardGeneric("pixelData"))
setGeneric("pixelData<-", function(object, value) standardGeneric("pixelData<-"))
setGeneric("pixelNames", function(object) standardGeneric("pixelNames"))
setGeneric("pixelNames<-", function(object, value) standardGeneric("pixelNames<-"))

#### Pre-processing ####
## ---------------------
setGeneric("process", function(object, ...) standardGeneric("process"))
setGeneric("reduceBaseline", function(object, ...) standardGeneric("reduceBaseline"))
setGeneric("peakPick", function(object, ...) standardGeneric("peakPick"))
setGeneric("peakAlign", function(object, ref, ...) standardGeneric("peakAlign"))
setGeneric("peakFilter", function(object, ...) standardGeneric("peakFilter"))

#### Data alignment and matching ####
## ----------------------------------
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
setGeneric("crossValidate", function(.x, .y, .fun, ...) standardGeneric("crossValidate"))
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
setGeneric("writeImzML", function(object, ...) standardGeneric("writeImzML"))

####-----------------------------------------------------------####

