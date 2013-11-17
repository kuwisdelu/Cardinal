
#### implement the get/set methods for MSImageSet slots ####

# get the spectra slot
setMethod("spectra", "MSImageSet", function(object) object@spectra$spectra )

# set the spectra slot
setReplaceMethod("spectra", "MSImageSet", function(object,value) {
	object@spectra$spectra <- value
	validObject(object)
	return(object)
} )

# get the peaks slot
setMethod("peaks", "MSImageSet", function(object) object@peaks )

# set the peaks slot
setReplaceMethod("peaks", "MSImageSet", function(object,value) {
	object@peaks <- value
	validObject(object)
	return(object)
} )

# get the featureData slot
setMethod("featureData", "MSImageSet", function(object) object@featureData )

# set the featureData slot
setReplaceMethod("featureData", "MSImageSet", function(object,value) {
	object@featureData <- value
	validObject(object)
	return(object)
} )

# get the pixelData slot
setMethod("pixelData", "MSImageSet", function(object) object@pixelData )

# set the pixelData slot
setReplaceMethod("pixelData", "MSImageSet", function(object,value) {
	object@pixelData <- value
	object@metaData[["positionArray"]] <- generatePositionArray(object)
	validObject(object)
	return(object)
} )

# get the metaData slot
setMethod("metaData", "MSImageSet", function(object) object@metaData )

# set the metaData slot
setReplaceMethod("metaData", "MSImageSet", function(object,value) {
	object@metaData <- value
	validObject(object)
	return(object)
} )

#### implement the get/set methods for other classes ####

# get the peaks slot
setMethod("peaks", "MSPeakFrame", function(object) object@peaks )

# set the peaks slot
setReplaceMethod("peaks", "MSPeakFrame", function(object,value) {
	object@peaks <- value
	validObject(object)
	return(object)
} )

# get the peaks slot
setMethod("peaks", "MSPeakList", function(object) object@peaks )

# set the peaks slot
setReplaceMethod("peaks", "MSPeakList", function(object,value) {
	object@peaks <- value
	validObject(object)
	return(object)
} )

# get the metaData slot
setMethod("metaData", "MSPeakFrame", function(object) object@metaData )

# set the metaData slot
setReplaceMethod("metaData", "MSPeakFrame", function(object,value) {
	object@metaData <- value
	validObject(object)
	return(object)
} )

# get the metaData slot
setMethod("metaData", "MSPeakList", function(object) object@metaData )

# set the metaData slot
setReplaceMethod("metaData", "MSPeakList", function(object,value) {
	object@metaData <- value
	validObject(object)
	return(object)
} )

# get the metaData slot
setMethod("metaData", "MSImageSegmentation", function(object) object@metaData )

# set the metaData slot
setReplaceMethod("metaData", "MSImageSegmentation", function(object,value) {
	object@metaData <- value
	validObject(object)
	return(object)
} )



