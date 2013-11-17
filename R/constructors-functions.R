
#### create a new MSImageSet ####

createMSImageSet <- function(spectra, mz, coord) {
	if ( missing(coord) ) {
		warning("no coordinates found")
		coord <- expand.grid(1:ncol(spectra), 1)
	}
	metaData <- generateMetaData("MSImageSet")
	if ( is.null(names(mz)) ) {
		metaData[["mzDimName"]] <- "mz"
	} else {
		metaData[["mzDimName"]] <- names(mz)
	}
	if ( is.null(names(coord)) ) {
		metaData[["coordDimNames"]] <- paste("x", 1:ncol(coord), sep="")
	} else {
		metaData[["coordDimNames"]] <- names(coord)
	}
	env <- new.env(parent=globalenv())
	if ( any(dim(spectra) != c(length(unlist(mz)), nrow(coord))) ) {
		stop("spectra has wrong dimensions")
	} else {
		env$spectra <- spectra
	}
	featureData <- as.data.frame(mz)
	names(featureData) <- metaData[["mzDimName"]]
	pixelData <- as.data.frame(coord)
	names(pixelData) <- metaData[["coordDimNames"]]
	metaData[["positionArray"]] <- generatePositionArray(coord)
	object <- new("MSImageSet", spectra=env, peaks=emptyMSPeakFrame(),
		featureData=featureData, pixelData=pixelData, metaData=metaData)
	return(object)
}

#### create a new MSPeakFrame ####

createMSPeakFrame <- function(mz, resolution) {
	fwhm <- mz / resolution
	sigma <- fwhm / (2 * sqrt(2 * log(2)))
	lbound <- mz - (fwhm / 2)
	ubound <- mz + (fwhm / 2)
	peaks <- data.frame(mz=mz, intensity=1, mu=mz, sigma=sigma, area=1,
		fwhm=fwhm, lbound=lbound, ubound=ubound)
	new("MSPeakFrame", peaks=peaks, metaData=generateMetaData("MSPeakFrame"))
}

emptyMSPeakFrame <- function() {
	peak.frame <- data.frame(mz=numeric(), intensity=numeric(), mu=numeric(),
		sigma=numeric(), area=numeric(), fwhm=numeric(),
		lbound=numeric(), ubound=numeric())
	new("MSPeakFrame", peaks=peak.frame, metaData=generateMetaData("MSPeakFrame"))
}

#### create a new MSPeakList ####

createMSPeakList <- function(peaklist) {
	if ( !is.list(peaklist) || !all(sapply(peaklist, isMSPeakFrame)) ) {
		stop("'peaklist' must be a list of 'MSPeakFrame' objects")
	}
	new("MSPeakList", peaks=peaklist, metaData=generateMetaData("MSPeakFrame"))
}

#### helper functions for construction ####

generateMetaData <- function(class) {
	object <- new(class)
	object@metaData
}

generateHistory <- function(call) {
	parent <- parent.frame()
	method <- as.character(call[[1]])
	if ( length(call) > 2 ) {
		argsymbols <- as.list(call)[-c(1,2)]
		argnames <- names(argsymbols)
		args <- lapply(argsymbols, function(e) {
			for ( i in seq_along(sys.frames()) ) {
				val <- try(eval(e, envir=sys.frame(-i)), silent=TRUE)
				if ( !inherits(val, "try-error") ) break
			}
			val
		} )
		names(args) <- argnames
		special <- sapply(args, is.function)
		args[special] <- argsymbols[special]
	} else {
		args <- NULL
	}
	list(method=method, call=call, args=args)
}

# mergeMetaData <- function(input, base) {
# 	ni <- names(input)[nchar(input) > 0]
# 	nb <- names(base)[nchar(base) > 0]
# 	dup <- sapply(ni, function(n) n %in% nb)	
# 	ni <- ni[dup]
# 	duplist <- sapply(ni, function(n) is.list(input[[n]]) && is.list(base[[n]]))
# 	mlist <- lapply(ni[duplist], function(n) {
# 		if ( is.data.frame(input[[n]]) && is.data.frame(base[[n]]) ) {
# 			return(rbind(base[[n]], input[[n]]))
# 		} else {
# 			return(c(base[[n]], input[[n]]))
# 		}
# 	} )
# 	merged <- c(input[!dup], base)
# 	merged[ni[duplist]] <- mlist
# 	return(merged)
# }

