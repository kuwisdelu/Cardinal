
#### implement features methods ####

# access the position of pixels in the spectra slot by their coordinates
setMethod("features", c("MSImageSet", "numeric"), function(object, mz, ...) {
	return(vapply(mz, function(mzi) which.min(abs(mzi - mz(object))), 1))
} )

