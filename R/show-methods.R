
#### implement the show methods ####

setMethod("show", "MSImageSet", function(object) {
	cat("S4 class              :", class(object), "\n")
	cat("sample name           :", object@metaData[["name"]], "\n")
	cat("number of features    :", numFeatures(object), "\n")
	cat("range of m/z values   :", min(mz(object)), "to", max(mz(object)), "\n")
	cat("range of m/z intervals:", min(diff(mz(object))), "to", max(diff(mz(object))), "\n")
	cat("number of pixels      :", numPixels(object), "\n")
	cat("number of dimensions  :", ncol(coord(object)), "\n")
	cat("dimensions of image   :", sapply(coord(object), max), "\n")
	cat("size of dataset       : ", object.size(object@spectra$spectra), " bytes (",
		round(object.size(object@spectra$spectra)/(2^20), digits=1),
		" MB in memory)\n", sep="")
	cat("most recent operation : ")
	if ( length(object@metaData[["history"]]) > 0 ) {
		cat(object@metaData[["history"]][[length(object@metaData[["history"]])]]$method,
			"\n", sep="")
	} else {
		cat("\n")
	}
} )

setMethod("show", "MSPeakFrame", function(object) {
	cat("S4 class :", class(object), "\n\n")
	show(object@peaks)
} )

setMethod("show", "MSPeakList", function(object) {
	cat("S4 class :", class(object), "\n\n")
	show(object@peaks)
} )

setMethod("show", "MSImageSegmentation", function(object) {
	cat("S4 class      :", class(object), "\n")
	cat("sample name   :", object@metaData[["name"]], "\n\n")
	for ( i in 1:metaData(object)$npar ) {
		cat("\t[[", i, "]]\n")
		cat("\t", "parameters            :" , object@metaData$parnames[[i]], "\n")
		cat("\t", "number of classes     :" , object$nclasses[[i]], "\n")
		cat("\t", "avg features per class:" , round(mean(object$nfeatures[[i]]), digits=1), "\n")
		cat("\t", "degrees of freedom    :" , object$dof[[i]], "\n\n")
	}
} )

