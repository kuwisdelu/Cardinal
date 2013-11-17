
#### read data types ####

readAnalyze7.5 <- function(name, folder=".") {
	# check for the files
	files <- list.files(folder)
	if ( ! paste(name, ".hdr", sep="") %in% files ) {
		stop("header file is missing")
	} else if ( ! paste(name, ".t2m", sep="") %in% files ) {
		stop("t2m file is missing")
	} else if ( ! paste(name, ".img", sep="") %in% files ) {
		stop("image file is missing")
	}
	# set up the workspace
	wd <- getwd()
	nmz <- integer(1)
	xdim <- integer(1)
	ydim <- integer(1)
	xint <- integer(1)
	yint <- integer(1)
	# read the header file
	setwd(folder)
	tryCatch(out.hdr <- .C("read_MALDI_hdr", as.integer(nmz), as.integer(xdim),
		as.integer(ydim), as.integer(xint), as.integer(yint), name),
		error=function(e) stop("failed to read header file"), finally=setwd(wd))
	nmz <- out.hdr[[1]]
	xdim <- out.hdr[[2]]
	ydim <- out.hdr[[3]]
	# browser()
	mz <- numeric(nmz)
	# reading t2m file
	setwd(folder)
	tryCatch(out.t2m <- .C("read_MALDI_t2m", as.integer(nmz), as.double(mz), name),
		error=function(e) stop("failed to read t2m file"), finally=setwd(wd))
	mz <- out.t2m[[2]]
	intensities <- numeric(nmz * xdim * ydim)
	# try reading the image file
	setwd(folder)
	tryCatch(intensities <- .C("read_MALDI_img", as.integer(nmz), as.integer(xdim),
		as.integer(ydim), as.integer(intensities), name)[[4]],
		error=function(e) stop("failed to read image file"), finally=setwd(wd))
	dim(intensities) <- c(nmz, xdim * ydim)
	# clean up
	setwd(wd); gc()
	# create the MSImageSet
	coord <- expand.grid(x=1:xdim, y=1:ydim)
	object <- createMSImageSet(spectra=intensities, mz=mz, coord=coord)
	object@metaData[["name"]] <- name
	object@metaData[["folder"]] <- folder
	object
}
