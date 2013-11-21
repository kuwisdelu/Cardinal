
#### get/set slot methods ####

if (!isGeneric("spectra"))
	setGeneric("spectra", function(object) standardGeneric("spectra"))

if (!isGeneric("spectra<-"))
	setGeneric("spectra<-", function(object,value) standardGeneric("spectra<-"))

if (!isGeneric("peaks"))
	setGeneric("peaks", function(object) standardGeneric("peaks"))

if (!isGeneric("peaks<-"))
	setGeneric("peaks<-", function(object,value) standardGeneric("peaks<-"))

if (!isGeneric("featureData"))
	setGeneric("featureData", function(object) standardGeneric("featureData"))

if (!isGeneric("featureData<-"))
	setGeneric("featureData<-", function(object,value) standardGeneric("featureData<-"))

if (!isGeneric("pixelData"))
	setGeneric("pixelData", function(object) standardGeneric("pixelData"))

if (!isGeneric("pixelData<-"))
	setGeneric("pixelData<-", function(object,value) standardGeneric("pixelData<-"))

if (!isGeneric("metaData"))
	setGeneric("metaData", function(object) standardGeneric("metaData"))

if (!isGeneric("metaData<-"))
	setGeneric("metaData<-", function(object,value) standardGeneric("metaData<-"))

#### all other methods ####

if (!isGeneric("alignPeaks"))
	setGeneric("alignPeaks", function(peaks, reference, ...) standardGeneric("alignPeaks"))

if (!isGeneric("assessQuality"))
	setGeneric("assessQuality", function(object, ...) standardGeneric("assessQuality"))

if (!isGeneric("binSpectra"))
	setGeneric("binSpectra", function(object, peaks, ...) standardGeneric("binSpectra"))

if (!isGeneric("calibrateSegmentation"))
	setGeneric("calibrateSegmentation", function(object, reference, ...) standardGeneric("calibrateSegmentation"))

if (!isGeneric("coord"))
	setGeneric("coord", function(object) standardGeneric("coord"))

if (!isGeneric("coord<-"))
	setGeneric("coord<-", function(object, value) standardGeneric("coord<-"))

if (!isGeneric("coregisterImages"))
	setGeneric("coregisterImages", function(object, ...) standardGeneric("coregisterImages"))

if (!isGeneric("crop"))
	setGeneric("crop", function(object, ...) standardGeneric("crop"))

if (!isGeneric("crossValidate"))
	setGeneric("crossValidate", function(x, y, ...) standardGeneric("crossValidate"))

if (!isGeneric("detectPeaks"))
	setGeneric("detectPeaks", function(object, ...) standardGeneric("detectPeaks"))

if (!isGeneric("estimateBaseline"))
	setGeneric("estimateBaseline", function(object, ...) standardGeneric("estimateBaseline"))

if (!isGeneric("estimateNoise"))
    setGeneric("estimateNoise", function(object, ...) standardGeneric("estimateNoise"));

if (!isGeneric("features"))
	setGeneric("features", function(object, mz, ...) standardGeneric("features"))

if (!isGeneric("flipHorizontal"))
	setGeneric("flipHorizontal", function(object, ...) standardGeneric("flipHorizontal"))

if (!isGeneric("flipVertical"))
	setGeneric("flipVertical", function(object, ...) standardGeneric("flipVertical"))

if (!isGeneric("generatePositionArray"))
	setGeneric("generatePositionArray", function(object, ...) standardGeneric("generatePositionArray"))

if (!isGeneric("image")) setGeneric("image")

if (!isGeneric("intensities"))
	setGeneric("intensities", function(object, ...) standardGeneric("intensities"))

if (!isGeneric("isBinned"))
	setGeneric("isBinned", function(object) standardGeneric("isBinned"))

if (!isGeneric("isBinned<-"))
	setGeneric("isBinned<-", function(object,value) standardGeneric("isBinned<-"))

if (!isGeneric("isPeaks"))
	setGeneric("isPeaks", function(object) standardGeneric("isPeaks"))

if (!isGeneric("isPeaks<-"))
	setGeneric("isPeaks<-", function(object,value) standardGeneric("isPeaks<-"))

if (!isGeneric("isResampled"))
	setGeneric("isResampled", function(object) standardGeneric("isResampled"))

if (!isGeneric("isResampled<-"))
	setGeneric("isResampled<-", function(object,value) standardGeneric("isResampled<-"))

if (!isGeneric("likPlot"))
	setGeneric("likPlot", function(object, ...) standardGeneric("likPlot"))

if (!isGeneric("mergePeaks"))
	setGeneric("mergePeaks", function(peaks, ...) standardGeneric("mergePeaks"))

if (!isGeneric("mz"))
	setGeneric("mz", function(object) standardGeneric("mz"))

if (!isGeneric("mz<-"))
	setGeneric("mz<-", function(object, value) standardGeneric("mz<-"))

if (!isGeneric("neighbors"))
	setGeneric("neighbors", function(object, r, ...) standardGeneric("neighbors"))

if (!isGeneric("numFeatures"))
	setGeneric("numFeatures", function(object) standardGeneric("numFeatures"))

if (!isGeneric("numPixels"))
	setGeneric("numPixels", function(object) standardGeneric("numPixels"))

if (!isGeneric("OPLS"))
	setGeneric("OPLS", function(x, y, ...) standardGeneric("OPLS"))

if (!isGeneric("PCA"))
	setGeneric("PCA", function(x, ...) standardGeneric("PCA"))

if (!isGeneric("pixels"))
	setGeneric("pixels", function(object, coord, ...) standardGeneric("pixels"))

if (!isGeneric("PLS"))
	setGeneric("PLS", function(x, y, ...) standardGeneric("PLS"))

if (!isGeneric("poolPeaks"))
	setGeneric("poolPeaks", function(peaks, ...) standardGeneric("poolPeaks"))

if (!isGeneric("predict"))
	setGeneric("predict", function(object, ...) standardGeneric("predict"))

if (!isGeneric("regeneratePositions"))
	setGeneric("regeneratePositions", function(object) standardGeneric("regeneratePositions"))

if (!isGeneric("removeBaseline"))
	setGeneric("removeBaseline", function(object, ...) standardGeneric("removeBaseline"))

if (!isGeneric("removeNoise"))
	setGeneric("removeNoise", function(object, ...) standardGeneric("removeNoise"))

if (!isGeneric("resampleSpectra"))
	setGeneric("resampleSpectra", function(object, peaks, ...) standardGeneric("resampleSpectra"))

if (!isGeneric("roiBind"))
	setGeneric("roiBind", function(object, ...) standardGeneric("roiBind"))

if (!isGeneric("rotateLeft"))
	setGeneric("rotateLeft", function(object, ...) standardGeneric("rotateLeft"))

if (!isGeneric("rotateRight"))
	setGeneric("rotateRight", function(object, ...) standardGeneric("rotateRight"))

if (!isGeneric("selectPeaks"))
	setGeneric("selectPeaks", function(object, ...) standardGeneric("selectPeaks"))

if (!isGeneric("selectPixels"))
	setGeneric("selectPixels", function(object, ...) standardGeneric("selectPixels"))

if (!isGeneric("selectROI"))
	setGeneric("selectROI", function(object, ...) standardGeneric("selectROI"))

if (!isGeneric("sliceBind"))
    setGeneric("sliceBind", function(object, ...) standardGeneric("sliceBind"));

if (!isGeneric("spatialClassify"))
    setGeneric("spatialClassify", function(object, ...) standardGeneric("spatialClassify"))

if (!isGeneric("spatialCluster"))
    setGeneric("spatialCluster", function(object, ...) standardGeneric("spatialCluster"))

if (!isGeneric("spatialKMeans"))
    setGeneric("spatialKMeans", function(object, ...) standardGeneric("spatialKMeans"))

if (!isGeneric("spatialSparseClassify"))
    setGeneric("spatialSparseClassify", function(object, labels, ...) standardGeneric("spatialSparseClassify"))

if (!isGeneric("spatialSparseCluster"))
    setGeneric("spatialSparseCluster", function(object, ...) standardGeneric("spatialSparseCluster"))

if (!isGeneric("spatialSparseKMeans"))
    setGeneric("spatialSparseKMeans", function(object, ...) standardGeneric("spatialSparseKMeans"))

if (!isGeneric("spectralApply"))
	setGeneric("spectralApply", function(object, MARGIN, FUN, ...) standardGeneric("spectralApply"))

if (!isGeneric("standardizeTotalImageIntensity"))
    setGeneric("standardizeTotalImageIntensity", function(object, ...) standardGeneric("standardizeTotalImageIntensity"));

if (!isGeneric("standardizeTotalIonCurrent"))
    setGeneric("standardizeTotalIonCurrent", function(object, ...) standardGeneric("standardizeTotalIonCurrent"))

if (!isGeneric("summaryPlot"))
	setGeneric("summaryPlot", function(object, reference, ...) standardGeneric("summaryPlot"))

if (!isGeneric("trellisImage"))
	setGeneric("trellisImage", function(x, ...) standardGeneric("trellisImage"))

if (!isGeneric("trellisPlot"))
	setGeneric("trellisPlot", function(x, ...) standardGeneric("trellisPlot"))

if (!isGeneric("trellisVolume"))
	setGeneric("trellisVolume", function(x, ...) standardGeneric("trellisVolume"))	

if (!isGeneric("unload"))
	setGeneric("unload", function(object, ...) standardGeneric("unload"))

if (!isGeneric("volume"))
	setGeneric("volume", function(x, ...) standardGeneric("volume"))

#### end all generics ####
