
#### read Analyze 7.5 files ####

readAnalyze <- function(name, folder) {
	# check for files
	hdrpath <- file.path(folder, paste(name, ".hdr", sep=""))
	if ( !file.exists(hdrpath) ) stop(hdrpath, " does not exist")
	t2mpath <- file.path(folder, paste(name, ".t2m", sep=""))
	if ( !file.exists(t2mpath) ) stop(t2mpath, " does not exist")
	imgpath <- file.path(folder, paste(name, ".img", sep=""))
	if ( !file.exists(t2mpath) ) stop(imgpath, " does not exist")
	# parse header and read m/z values
	hdr <- .Call("readAnalyzeHDR", hdrpath)
	mz <- .Call("readAnalyzeT2M", t2mpath, hdr$dime$dim[[2]])
	dim <- as.integer(c(hdr$dime$dim[[2]], prod(hdr$dime$dim[c(3,4,5)])))
	# read data from image file
	data <- NULL
	if ( hdr$dime$datatype == 4 ) # short
		data <- .Call("readAnalyzeSIMG", imgpath, dim, NULL)
	if ( hdr$dime$datatype == 8 ) # int
		data <- .Call("readAnalyzeIIMG", imgpath, dim, NULL)
	if ( hdr$dime$datatype == 16 ) # float
		data <- .Call("readAnalyzeFIMG", imgpath, dim, NULL)
	if ( hdr$dime$datatype == 64 ) # double
		data <- .Call("readAnalyzeDIMG", imgpath, dim, NULL)
	if ( is.null(data) ) stop("unknown data type")
	# set up coordinates
	if ( hdr$dime$dim[[5]] > 1 ) {
		coord <- expand.grid(x=seq_len(hdr$dime$dim[[3]]),
			y=seq_len(hdr$dime$dim[[4]]), z=seq_len(hdr$dime$dim[[5]]))
	} else {
		coord <- expand.grid(x=seq_len(hdr$dime$dim[[3]]),
			y=seq_len(hdr$dime$dim[[4]]))
	}
	# create and return dataset
	MSImageSet(spectra=data, mz=mz, coord=coord)
}
