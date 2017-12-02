
#### Write MSI data to any file ####
## -------------------------------

setMethod("writeMSIData",
	signature = c("MSImageSet", "character"),
	function(object, file, outformat = "imzML", ...)
	{
		path <- normalizePath(file, mustWork=FALSE)
		name <- basename(file_path_sans_ext(path))
		folder <- dirname(path)
		if ( outformat == "imzML" ) {
			result <- writeImzML(object, name, folder, ...)
		} else {
			.stop("only outformat 'imzML' is supported")
		}
		invisible(result)
	})
