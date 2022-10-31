
#### Write Analyze files ####
## ----------------------

writeAnalyze <- function(object, name, folder = getwd(),
		intensity.type = "16-bit integer", ...)
	{
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
		# check if gridded coordinates (always true for MSImageSet)
		if ( is(object, "MSImagingExperiment") && !gridded(object) )
			.stop("writing Analyze format only supports gridded data")
		# check for files
		hdrpath <- normalizePath(file.path(folder, paste(name, ".hdr", sep="")),
			mustWork=FALSE)
		if ( file.exists(hdrpath) ) .stop("file ", hdrpath, " already exists")
		t2mpath <- normalizePath(file.path(folder, paste(name, ".t2m", sep="")),
			mustWork=FALSE)
		if ( file.exists(t2mpath) ) .stop("file ", t2mpath, " already exists")
		imgpath <- normalizePath(file.path(folder, paste(name, ".img", sep="")),
			mustWork=FALSE)
		if ( file.exists(imgpath) ) .stop("file ", imgpath, " already exists")
		# pixel datatype
		datatype <- Ctypeof(intensity.type)
		# write image file
		.message("writing img file '", imgpath, "'")
		img <- .writeAnalyzeIMG(object, imgpath, datatype)
		# write m/z values
		.message("writing t2m file '", t2mpath, "'")
		t2m <- .writeAnalyzeT2M(object, t2mpath)
		# write header
		.message("writing header file '", hdrpath, "'")
		hdr <- .writeAnalyzeHDR(object, hdrpath, datatype)
		result <- TRUE
		if ( result )
			.message("done.")
		invisible(result)
	}

.writeAnalyzeHDR <- function(x, file, type) {
	file <- normalizePath(file, mustWork=FALSE)
	if ( !file.create(file) )
		.stop(paste0("couldn't create file '", file, "'"))
	warn <- getOption("matter.cast.warning")
	options(matter.cast.warning=FALSE)
	hdr <- .makeAnalyzeHDR(file, mode="rw")
	hdr$hk[] <- rep(list(0), length(hdr$hk))
	hdr$hk$sizeof_hdr <- 348L # byte size of hdr
	hdr$hk$extents <- 16384L # must be 16384
	hdr$hk$regular <- charToRaw("r")
	hdr$dime[] <- rep(list(0), length(hdr$dime))
	if ( is3D(x) ) {
		dim <- sapply(coord(x)[,1:3], max)
		hdr$dime$dim[1:5] <- c(4L, nrow(x), dim[1], dim[2], dim[3])
	} else {
		dim <- sapply(coord(x)[,c(1,2)], max)
		hdr$dime$dim[1:5] <- c(4L, nrow(x), dim[1], dim[2], 1L)
	}
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
	t2m <- matter_vec(type="float", path=file,
		length=length(mz(x)), readonly=FALSE)
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
	if ( is3D(x) ) {
		dim <- sapply(coord(x)[,1:3], max)
		img <- matter_mat(type=type, path=file,
			nrow=nrow(x), ncol=prod(dim), readonly=FALSE)
		img[] <- 0
		for ( i in seq_len(ncol(x)) ) {
			row <- coord(x)[i,1]
			col <- coord(x)[i,2]
			dep <- coord(x)[i,3]
			j <- row + dim[1] * (col - 1) + prod(dim[c(1,2)]) * (dep - 1)
			img[,j] <- iData(x)[,i]
		}
	} else {
		dim <- sapply(coord(x)[,c(1,2)], max)
		img <- matter_mat(type=type, path=file,
			nrow=nrow(x), ncol=prod(dim), readonly=FALSE)
		img[] <- 0
		for ( i in seq_len(ncol(x)) ) {
			row <- coord(x)[i,1]
			col <- coord(x)[i,2]
			j <- row + dim[1] * (col - 1)
			img[,j] <- iData(x)[,i]
		}
	}
	options(matter.cast.warning=warn)
	img
}

