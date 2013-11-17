
#### implement numFeatures methods ####

# get the number of features
setMethod("numFeatures", "MSImageSet", function(object) nrow(object@spectra$spectra) )
