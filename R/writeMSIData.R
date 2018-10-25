
#### Write MSI data to any file ####
## -------------------------------

writeMSIData <- function(object, file, outformat = c("imzML", "Analyze"), ...) {
	path <- normalizePath(file, mustWork=FALSE)
	name <- basename(file_path_sans_ext(path))
	folder <- dirname(path)
	outformat <- match.arg(outformat)
	if ( outformat == "imzML" ) {
		result <- writeImzML(object, name, folder, ...)
	} else if ( outformat == "Analyze" ) {
		result <- writeAnalyze(object, name, folder, ...)
	} else {
		.stop("writeMSIData: only 'imzML' and 'Analyze' are supported")
	}
	invisible(result)
}
