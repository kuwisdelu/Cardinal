
#### Read Analyze 7.5 files ####
## ----------------------------

readAnalyze <- function(name, folder=getwd(), attach.only=FALSE, ...)
{
	# check for files
	hdrpath <- normalizePath(file.path(folder, paste(name, ".hdr", sep="")),
		mustWork=FALSE)
	if ( !file.exists(hdrpath) ) .stop("readAnalyze: ", hdrpath, " does not exist")
	t2mpath <- normalizePath(file.path(folder, paste(name, ".t2m", sep="")),
		mustWork=FALSE)
	if ( !file.exists(t2mpath) ) .stop("readAnalyze: ", t2mpath, " does not exist")
	imgpath <- normalizePath(file.path(folder, paste(name, ".img", sep="")),
		mustWork=FALSE)
	if ( !file.exists(imgpath) ) .stop("readAnalyze: ", imgpath, " does not exist")
	# parse header
	.log("readAnalyze: Reading header file '", hdrpath, "'")
	hdr <- .Call("readAnalyzeHDR", hdrpath)
	dim <- as.integer(c(hdr$dime$dim[[2]], prod(hdr$dime$dim[c(3,4,5)])))
	datatype <- as.integer(hdr$dime$datatype)
	# read m/z values
	.log("readAnalyze: Reading T2M file '", t2mpath, "'")
	mz <- .Call("readAnalyzeT2M", t2mpath, hdr$dime$dim[[2]])
	# read image file
	.log("readAnalyze: Reading IMG file '", imgpath, "'")
	if ( attach.only ) {
		if ( "package:matter" %in% search() ) {
			datamode <- switch(as.character(datatype),
				`4` = "short",
				`8` = "int",
				`16` = "float",
				`64` = "double")
			data <- matter_mat(paths=imgpath, datamode=datamode, nrow=dim[1], ncol=dim[2])
		} else {
			datatype <- switch(as.character(datatype),
				`4` = "16-bit integer",
				`8` = "32-bit integer",
				`16` = "32-bit float",
				`64` = "64-bit float")
			data <- Binmat(files=imgpath, datatype=datatype, nrow=dim[1], ncol=dim[2])
		}
	} else {
		data <- .Call("readAnalyzeIMG", imgpath, dim, datatype)
	}
	# set up coordinates
	if ( hdr$dime$dim[[5]] > 1 ) {
		coord <- expand.grid(x=seq_len(hdr$dime$dim[[3]]),
			y=seq_len(hdr$dime$dim[[4]]), z=seq_len(hdr$dime$dim[[5]]))
	} else {
		coord <- expand.grid(x=seq_len(hdr$dime$dim[[3]]),
			y=seq_len(hdr$dime$dim[[4]]))
	}
	# create and return dataset
	experimentData <- new("MIAPE-Imaging")
	processingData <- new("MSImageProcess", files=c(hdrpath, t2mpath, imgpath))
	object <- MSImageSet(spectra=data, mz=mz, coord=coord,
		processingData=processingData,
		experimentData=experimentData)
	sampleNames(object) <- name
	object
}
