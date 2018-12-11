
#### Write Analyze files ####
## ----------------------

writeAnalyze <- function(object, name, folder = getwd(),
		intensity.type = "16-bit integer", ...)
	{
		if ( is(object, "MSImageSet") && length(sampleNames(object)) > 1 ) {
			samples <- sampleNames(object)
			result <- sapply(samples, function(nm) {
				tmp <- object[,pData(object)$sample == nm]
				name2 <- paste0(name, "-", nm)
				writeAnalyze(tmp, name2, folder,
					intensity.type=intensity.type, ...)
			})
			return(invisible(result))
		}
		if ( is(object, "MSImagingExperiment") && length(runNames(object)) > 1 ) {
			runs <- runNames(object)
			result <- sapply(runs, function(id) {
				tmp <- object[,run(object) == id]
				name2 <- paste0(name, "-", id)
				writeAnalyze(tmp, name2, folder,
					intensity.type=intensity.type, ...)
			})
			return(invisible(result))
		}
		# check for files
		hdrpath <- normalizePath(file.path(folder, paste(name, ".hdr", sep="")),
			mustWork=FALSE)
		if ( file.exists(hdrpath) ) .stop("writeAnalyze: ", hdrpath, " already exists")
		t2mpath <- normalizePath(file.path(folder, paste(name, ".t2m", sep="")),
			mustWork=FALSE)
		if ( file.exists(t2mpath) ) .stop("writeAnalyze: ", t2mpath, " already exists")
		imgpath <- normalizePath(file.path(folder, paste(name, ".img", sep="")),
			mustWork=FALSE)
		if ( file.exists(imgpath) ) .stop("writeAnalyze: ", imgpath, " already exists")
		# pixel datatype
		datatype <- Ctypeof(intensity.type)
		# write image file
		.message("writeAnalyze: Writing IMG file '", imgpath, "'")
		img <- .writeAnalyzeIMG(object, imgpath, datatype)
		# write m/z values
		.message("writeAnalyze: Writing T2M file '", t2mpath, "'")
		t2m <- .writeAnalyzeT2M(object, t2mpath)
		# write header
		.message("writeAnalyze: Writing header file '", hdrpath, "'")
		hdr <- .writeAnalyzeHDR(object, hdrpath, datatype)
		result <- TRUE
		if ( result )
			.message("writeAnalyze: Done.")
		invisible(result)
	}

.writeAnalyzeHDR <- function(x, file, type) {
	file <- normalizePath(file, mustWork=FALSE)
	if ( !file.create(file) )
		.stop(paste0("couldn't create file '", file, "'"))
	warn <- getOption("matter.cast.warning")
	options(matter.cast.warning=FALSE)
	dim <- sapply(coord(x)[,c(1,2)], max)
	hdr <- .makeAnalyzeHDR(file, "rb+")
	hdr$hk[] <- rep(list(0), length(hdr$hk))
	hdr$hk$sizeof_hdr <- 348L # byte size of hdr
	hdr$hk$extents <- 16384L # must be 16384
	hdr$hk$regular <- charToRaw("r")
	hdr$dime[] <- rep(list(0), length(hdr$dime))
	hdr$dime$dim[1:5] <- c(4L, nrow(x), dim[1], dim[2], 1L)
	hdr$dime$datatype <- switch(as.character(type),
		"short" = 4L,
		"int" = 8L,
		"float" = 16L,
		"double" = 64L,
		stop("unrecognized img datatype"))
	hdr$dime$bitpix <- switch(as.character(type),
		"short" = 2L,
		"int" = 4L,
		"float" = 4L,
		"double" = 8L,
		stop("unrecognized img datatype"))
	hdr$dime$pixdim[1:4] <- rep(1L, 4)
	hdr$hist[] <- rep(list(0), length(hdr$hist))
	options(matter.cast.warning=warn)
	hdr
}

.writeAnalyzeT2M <- function(x, file) {
	file <- normalizePath(file, mustWork=FALSE)
	if ( !file.create(file) )
		.stop(paste0("couldn't create file '", file, "'"))
	warn <- getOption("matter.cast.warning")
	options(matter.cast.warning=FALSE)
	t2m <- matter_vec(datamode="float", paths=file,
		length=length(mz(x)), filemode="rb+")
	t2m[] <- mz(x)
	options(matter.cast.warning=warn)
	t2m
}

.writeAnalyzeIMG <- function(x, file, type) {
	file <- normalizePath(file, mustWork=FALSE)
	if ( !file.create(file) )
		.stop(paste0("couldn't create file '", file, "'"))
	warn <- getOption("matter.cast.warning")
	options(matter.cast.warning=FALSE)
	dim <- sapply(coord(x)[,c(1,2)], max)
	img <- matter_mat(datamode=type, paths=file,
		nrow=nrow(x), ncol=prod(dim), filemode="rb+")
	img[] <- 0
	for ( i in seq_len(ncol(x)) ) {
		row <- coord(x)[i,1]
		col <- coord(x)[i,2]
		j <- row + dim[1] * (col - 1)
		img[,j] <- iData(x)[,i]
	}
	options(matter.cast.warning=warn)
	img
}

