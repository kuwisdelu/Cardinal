
#### get/set slot methods ####

if (is.null(getGeneric("spectra")))
	setGeneric("spectra", function(object) standardGeneric("spectra"))

if (is.null(getGeneric("spectra<-")))
	setGeneric("spectra<-", function(object,value) standardGeneric("spectra<-"))

if (is.null(getGeneric("peaks")))
	setGeneric("peaks", function(object) standardGeneric("peaks"))

if (is.null(getGeneric("peaks<-")))
	setGeneric("peaks<-", function(object,value) standardGeneric("peaks<-"))

if (is.null(getGeneric("featureData")))
	setGeneric("featureData", function(object) standardGeneric("featureData"))

if (is.null(getGeneric("featureData<-")))
	setGeneric("featureData<-", function(object,value) standardGeneric("featureData<-"))

if (is.null(getGeneric("pixelData")))
	setGeneric("pixelData", function(object) standardGeneric("pixelData"))

if (is.null(getGeneric("pixelData<-")))
	setGeneric("pixelData<-", function(object,value) standardGeneric("pixelData<-"))

if (is.null(getGeneric("metaData")))
	setGeneric("metaData", function(object) standardGeneric("metaData"))

if (is.null(getGeneric("metaData<-")))
	setGeneric("metaData<-", function(object,value) standardGeneric("metaData<-"))

#### all other methods ####

if (is.null(getGeneric("$")))
	setGeneric("$", function(x, name) standardGeneric("$"))

if (is.null(getGeneric("$<-")))
	setGeneric("$<-", function(x, name, value) standardGeneric("$<-"))

if (is.null(getGeneric("[")))
	setGeneric("[", function(x, i, j, drop) standardGeneric("["))

if (is.null(getGeneric("[<-")))
	setGeneric("[<-", function(x, i, j, value) standardGeneric("[<-"))

if (is.null(getGeneric("[[")))
	setGeneric("[[", function(x, i) standardGeneric("[["))

if (is.null(getGeneric("[[<-")))
	setGeneric("[[<-", function(x, i, value) standardGeneric("[[<-"))

if (is.null(getGeneric("alignPeaks")))
	setGeneric("alignPeaks", function(peaks, reference, ...) standardGeneric("alignPeaks"))

if (is.null(getGeneric("assessQuality")))
	setGeneric("assessQuality", function(object, ...) standardGeneric("assessQuality"))

if (is.null(getGeneric("binSpectra")))
	setGeneric("binSpectra", function(object, peaks, ...) standardGeneric("binSpectra"))

if (is.null(getGeneric("calibrateSegmentation")))
	setGeneric("calibrateSegmentation", function(object, reference, ...) standardGeneric("calibrateSegmentation"))

if (is.null(getGeneric("coord")))
	setGeneric("coord", function(object) standardGeneric("coord"))

if (is.null(getGeneric("coord<-")))
	setGeneric("coord<-", function(object, value) standardGeneric("coord<-"))

if (is.null(getGeneric("coregisterImages")))
	setGeneric("coregisterImages", function(object, ...) standardGeneric("coregisterImages"))

if (is.null(getGeneric("crop")))
	setGeneric("crop", function(object, ...) standardGeneric("crop"))

if (is.null(getGeneric("crossValidate")))
	setGeneric("crossValidate", function(x, y, ...) standardGeneric("crossValidate"))

if (is.null(getGeneric("detectPeaks")))
	setGeneric("detectPeaks", function(object, ...) standardGeneric("detectPeaks"))

if (is.null(getGeneric("estimateBaseline")))
	setGeneric("estimateBaseline", function(object, ...) standardGeneric("estimateBaseline"))

if (is.null(getGeneric("estimateNoise")))
    setGeneric("estimateNoise", function(object, ...) standardGeneric("estimateNoise"));

if (is.null(getGeneric("features")))
	setGeneric("features", function(object, mz, ...) standardGeneric("features"))

if (is.null(getGeneric("flipHorizontal")))
	setGeneric("flipHorizontal", function(object, ...) standardGeneric("flipHorizontal"))

if (is.null(getGeneric("flipVertical")))
	setGeneric("flipVertical", function(object, ...) standardGeneric("flipVertical"))

if (is.null(getGeneric("generatePositionArray")))
	setGeneric("generatePositionArray", function(object, ...) standardGeneric("generatePositionArray"))

if (is.null(getGeneric("image")))
	setGeneric("image", function(x, ...) standardGeneric("image"))

if (is.null(getGeneric("intensities")))
	setGeneric("intensities", function(object, ...) standardGeneric("intensities"))

if (is.null(getGeneric("isBinned")))
	setGeneric("isBinned", function(object) standardGeneric("isBinned"))

if (is.null(getGeneric("isBinned<-")))
	setGeneric("isBinned<-", function(object,value) standardGeneric("isBinned<-"))

if (is.null(getGeneric("isPeaks")))
	setGeneric("isPeaks", function(object) standardGeneric("isPeaks"))

if (is.null(getGeneric("isPeaks<-")))
	setGeneric("isPeaks<-", function(object,value) standardGeneric("isPeaks<-"))

if (is.null(getGeneric("isResampled")))
	setGeneric("isResampled", function(object) standardGeneric("isResampled"))

if (is.null(getGeneric("isResampled<-")))
	setGeneric("isResampled<-", function(object,value) standardGeneric("isResampled<-"))

if (is.null(getGeneric("likPlot")))
	setGeneric("likPlot", function(object, ...) standardGeneric("likPlot"))

if (is.null(getGeneric("logLik")))
	setGeneric("logLik", function(object, ...) standardGeneric("logLik"))

if (is.null(getGeneric("mergePeaks")))
	setGeneric("mergePeaks", function(peaks, ...) standardGeneric("mergePeaks"))

if (is.null(getGeneric("mz")))
	setGeneric("mz", function(object) standardGeneric("mz"))

if (is.null(getGeneric("mz<-")))
	setGeneric("mz<-", function(object, value) standardGeneric("mz<-"))

if (is.null(getGeneric("neighbors")))
	setGeneric("neighbors", function(object, r, ...) standardGeneric("neighbors"))

if (is.null(getGeneric("numFeatures")))
	setGeneric("numFeatures", function(object) standardGeneric("numFeatures"))

if (is.null(getGeneric("numPixels")))
	setGeneric("numPixels", function(object) standardGeneric("numPixels"))

if (is.null(getGeneric("OPLS")))
	setGeneric("OPLS", function(x, y, ...) standardGeneric("OPLS"))

if (is.null(getGeneric("PCA")))
	setGeneric("PCA", function(x, ...) standardGeneric("PCA"))

if (is.null(getGeneric("pixels")))
	setGeneric("pixels", function(object, coord, ...) standardGeneric("pixels"))

if (is.null(getGeneric("PLS")))
	setGeneric("PLS", function(x, y, ...) standardGeneric("PLS"))

if (is.null(getGeneric("plot")))
	setGeneric("plot", function(x, y, ...) standardGeneric("plot"))

if (is.null(getGeneric("poolPeaks")))
	setGeneric("poolPeaks", function(peaks, ...) standardGeneric("poolPeaks"))

if (is.null(getGeneric("predict")))
	setGeneric("predict", function(object, ...) standardGeneric("predict"))

if (is.null(getGeneric("regeneratePositions")))
	setGeneric("regeneratePositions", function(object) standardGeneric("regeneratePositions"))

if (is.null(getGeneric("removeBaseline")))
	setGeneric("removeBaseline", function(object, ...) standardGeneric("removeBaseline"))

if (is.null(getGeneric("removeNoise")))
	setGeneric("removeNoise", function(object, ...) standardGeneric("removeNoise"))

if (is.null(getGeneric("resampleSpectra")))
	setGeneric("resampleSpectra", function(object, peaks, ...) standardGeneric("resampleSpectra"))

if (is.null(getGeneric("roiBind")))
	setGeneric("roiBind", function(object, ...) standardGeneric("roiBind"))

if (is.null(getGeneric("rotateLeft")))
	setGeneric("rotateLeft", function(object, ...) standardGeneric("rotateLeft"))

if (is.null(getGeneric("rotateRight")))
	setGeneric("rotateRight", function(object, ...) standardGeneric("rotateRight"))

if (is.null(getGeneric("show")))
	setGeneric("show", function(object) standardGeneric("show"))

if (is.null(getGeneric("selectPeaks")))
	setGeneric("selectPeaks", function(object, ...) standardGeneric("selectPeaks"))

if (is.null(getGeneric("selectPixels")))
	setGeneric("selectPixels", function(object, ...) standardGeneric("selectPixels"))

if (is.null(getGeneric("selectROI")))
	setGeneric("selectROI", function(object, ...) standardGeneric("selectROI"))

if (is.null(getGeneric("sliceBind")))
    setGeneric("sliceBind", function(object, ...) standardGeneric("sliceBind"));

if (is.null(getGeneric("spatialClassify")))
    setGeneric("spatialClassify", function(object, ...) standardGeneric("spatialClassify"))

if (is.null(getGeneric("spatialCluster")))
    setGeneric("spatialCluster", function(object, ...) standardGeneric("spatialCluster"))

if (is.null(getGeneric("spatialKMeans")))
    setGeneric("spatialKMeans", function(object, ...) standardGeneric("spatialKMeans"))

if (is.null(getGeneric("spatialSparseClassify")))
    setGeneric("spatialSparseClassify", function(object, labels, ...) standardGeneric("spatialSparseClassify"))

if (is.null(getGeneric("spatialSparseCluster")))
    setGeneric("spatialSparseCluster", function(object, ...) standardGeneric("spatialSparseCluster"))

if (is.null(getGeneric("spatialSparseKMeans")))
    setGeneric("spatialSparseKMeans", function(object, ...) standardGeneric("spatialSparseKMeans"))

if (is.null(getGeneric("spectralApply")))
	setGeneric("spectralApply", function(object, MARGIN, FUN, ...) standardGeneric("spectralApply"))

if (is.null(getGeneric("standardizeTotalImageIntensity")))
    setGeneric("standardizeTotalImageIntensity", function(object, ...) standardGeneric("standardizeTotalImageIntensity"));

if (is.null(getGeneric("standardizeTotalIonCurrent")))
    setGeneric("standardizeTotalIonCurrent", function(object, ...) standardGeneric("standardizeTotalIonCurrent"))

if (is.null(getGeneric("summary")))
	setGeneric("summary", function(object, ...) standardGeneric("summary"))

if (is.null(getGeneric("summaryPlot")))
	setGeneric("summaryPlot", function(object, reference, ...) standardGeneric("summaryPlot"))

if (is.null(getGeneric("trellisImage")))
	setGeneric("trellisImage", function(x, ...) standardGeneric("trellisImage"))

if (is.null(getGeneric("trellisPlot")))
	setGeneric("trellisPlot", function(x, ...) standardGeneric("trellisPlot"))

if (is.null(getGeneric("trellisVolume")))
	setGeneric("trellisVolume", function(x, ...) standardGeneric("trellisVolume"))	

if (is.null(getGeneric("unload")))
	setGeneric("unload", function(object, ...) standardGeneric("unload"))

if (is.null(getGeneric("volume")))
	setGeneric("volume", function(x, ...) standardGeneric("volume"))

#### end all generics ####
