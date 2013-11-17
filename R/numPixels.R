
#### implement numPixels methods ####

# get the number of pixels
setMethod("numPixels", "MSImageSet", function(object) ncol(object@spectra$spectra) )

