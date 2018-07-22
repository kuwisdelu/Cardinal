
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
	hdr <- .readAnalyzeHDR(hdrpath)
	dim <- as.integer(c(hdr$dime$dim[2], hdr$dime$dim[c(3,4,5)]))
	sdim <- c(dim[1], prod(dim[c(2,3,4)]))
	datatype <- as.integer(hdr$dime$datatype)
	# read m/z values
	.log("readAnalyze: Reading T2M file '", t2mpath, "'")
	mz <- .readAnalyzeT2M(t2mpath, n=dim[1])
	# read image file
	.log("readAnalyze: Reading IMG file '", imgpath, "'")
	type <- switch(as.character(datatype),
		`4` = "short",
		`8` = "int",
		`16` = "float",
		`64` = "double",
		stop("unrecognized img binary type"))
	spectra <- .readAnalyzeIMG(imgpath, dim=sdim, type=type)
	# set up coordinates
	if ( dim[4] > 1 ) {
		coord <- expand.grid(x=seq_len(dim[2]), y=seq_len(dim[3]), z=seq_len(dim[4]))
	} else {
		coord <- expand.grid(x=seq_len(dim[2]), y=seq_len(dim[3]))
	}
	files <- structure(c(hdrpath, t2mpath, imgpath), name=name, folder=folder)
	obj <- .readAnalyze.MSImageSet(spectra, mz, coord, name, files, attach.only)
	if ( validObject(obj) )
		obj
}

.readAnalyze.MSImageSet <- function(spectra, mz, coord, name, files, attach.only) {
	# set up spectra
	if ( attach.only ) {
		spectra <- spectra
	} else {
		spectra <- spectra[]
	}
	mz <- mz[]
	# create and return dataset
	experimentData <- new("MIAPE-Imaging")
	processingData <- new("MSImageProcess", files=files)
	object <- MSImageSet(spectra=spectra, mz=mz, coord=coord,
		processingData=processingData,
		experimentData=experimentData)
	sampleNames(object) <- name
	object
}

.makeAnalyzeHDR <- function(file, mode) {
	header_key <- struct(
		sizeof_hdr=c("int"=1),
		data_type=c("char"=10),
		db_name=c("char"=18),
		extents=c("int"=1),
		session_error=c("short"=1),
		regular=c("char"=1),
		hkey_un0=c("char"=1),
		filename=file, filemode=mode, offset=0)
	image_dimension <- struct(
		dim=c("short"=8),
		unused8=c("short"=1),
		unused9=c("short"=1),
		unused10=c("short"=1),
		unused11=c("short"=1),
		unused12=c("short"=1),
		unused13=c("short"=1),
		unused14=c("short"=1),
		datatype=c("short"=1),
		bitpix=c("short"=1),
		dim_un0=c("short"=1),
		pixdim=c("float"=8),
		vox_offset=c("float"=1),
		funused1=c("float"=1),
		funused2=c("float"=1),
		funused3=c("float"=1),
		cal_max=c("float"=1),
		cal_min=c("float"=1),
		compressed=c("float"=1),
		verified=c("float"=1),
		glmax=c("int"=1),
		glmin=c("int"=1),
		filename=file, filemode=mode, offset=40)
	data_history <- struct(
		descript=c("char"=80),
		aux_file=c("char"=24),
		orient=c("char"=1),
		originator=c("char"=10),
		generated=c("char"=10),
		scannum=c("char"=10),
		patient_id=c("char"=10),
		exp_date=c("char"=10),
		exp_time=c("char"=10),
		hist_un0=c("char"=3),
		views=c("int"=1),
		vols_added=c("int"=1),
		start_field=c("int"=1),
		field_skip=c("int"=1),
		omax=c("int"=1),
		omin=c("int"=1),
		smax=c("int"=1),
		smin=c("int"=1),
		filename=file, filemode=mode, offset=148)
	list(hk=header_key, dime=image_dimension, hist=data_history)
}

.readAnalyzeHDR <- function(file) {
	file <- normalizePath(file)
	hdr <- .makeAnalyzeHDR(file, "rb")
	hdr
}

.readAnalyzeT2M <- function(file, n) {
	file <- normalizePath(file)
	t2m <- matter_vec(datamode="float", paths=file, length=n)
	t2m
}

.readAnalyzeIMG <- function(file, dim, type) {
	file <- normalizePath(file)
	img <- matter_mat(datamode=type, paths=file, nrow=dim[1], ncol=dim[2])
	img
}
