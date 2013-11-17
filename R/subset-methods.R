
#### implement operator subset methods ####

#### set [] operator ####

setMethod("[", "MSImageSet", function(x, i, j, ..., drop=FALSE) {
	if ( missing(i) ) {
		feature <- 1:numFeatures(x)
	} else if ( is.numeric(i) ) {
		feature <- i
	} else if ( is.character(i) ) {
		if ( is.null(rownames(x@featureData)) ) stop("no rownames for featureData")
		feature <- 1:numFeatures(x)
		names(feature) <- rownames(x@featureData)
		feature <- feature[i]
	} else if ( is.logical(i) ) {
		feature <- which(rep(i, length.out=numFeatures(x)))
	} else {
		stop("illegal subscript")
	}
	coord <- coordFromCall(x, match.call(), missing(i), missing(j), missing(drop))
	intensities(x, feature=feature, coord=coord, drop=drop)
} )

setMethod("[", "MSPeakFrame", function(x, i, j, ..., drop=FALSE) {
	if ( missing(j) ) {
		peaks <- x@peaks[i,]
		rownames(peaks) <- NULL
		x.new <- new("MSPeakFrame", peaks=peaks, metaData=x@metaData)
		return(x.new)
	} else if ( missing(i) ) {
		return(x@peaks[, j, ..., drop=TRUE])
	} else {
		return(x@peaks[i, j, ..., drop=drop])
	}
} )

setMethod("[", "MSPeakList", function(x, i, j, ..., drop=FALSE) {
	peaks <- x@peaks[i]
	x.new <- new("MSPeakList", peaks=peaks, metaData=x@metaData)
	return(x.new)
} )

#### set []<- operator ####

# setReplaceMethod("[", c("MSImageSet"), function(x, i, j, ..., value) {
# 	if ( missing(i) ) {
# 		feature <- 1:numFeatures(x)
# 	} else if ( is.numeric(i) ) {
# 		feature <- i
# 	} else if ( is.character(i) ) {
# 		if ( is.null(rownames(x@featureData)) ) stop("no rownames for featureData")
# 		feature <- 1:numFeatures(x)
# 		names(feature) <- rownames(x@featureData)
# 		feature <- feature[i]
# 	} else if ( is.logical(i) ) {
# 		feature <- which(rep(i, length.out=numFeatures(x)))
# 	} else {
# 		stop("illegal subscript")
# 	}
# 	coord <- coordFromCall(x, match.call(), missing(i), missing(j), TRUE)
# 	pixel <- pixels(x, coord=coord)
# 	x@spectra$spectra[feature,pixel] <- value
# 	validObject(x)
# 	return(x)
# } )

#### set [[]] operator ####

setMethod("[[", c("MSImageSet", "character", "missing"), function(x, i, j, ...) {
	if ( length(i) > 1 ) stop("attempt to select more than one element")
	if ( !is.na(pmatch(i, names(x@featureData))) ) {
		x@featureData[,i]
	} else if ( !is.na(pmatch(i, names(x@pixelData))) ) {
		x@pixelData[,i]
	} else if ( !is.na(pmatch(i, names(x@metaData))) ) {
		x@metaData[[i]]
	} else {
		stop("could not find member matching ", substitute(name))
	}
} )

setMethod("[[", c("MSImageSet", "numeric", "missing"), function(x, i, j, ...) {
	if ( length(i) > 1 ) stop("attempt to select more than one element")
	return(x@spectra$spectra[,i])
} )

setMethod("[[", c("MSPeakFrame", "ANY", "missing"), function(x, i, j, ...) {
	if ( length(i) > 1 ) stop("attempt to select more than one element")
	return(x@peaks[[i]])
} )

setMethod("[[", c("MSPeakList", "ANY", "missing"), function(x, i, j, ...) {
	if ( length(i) > 1 ) stop("attempt to select more than one element")
	return(x@peaks[[i]])
} )

#### set [[]]<- operator ####

# setReplaceMethod("[[", c("MSImageSet", "character", "missing"), function(x, i, j, ..., value) {
# 	if ( length(i) > 1 ) stop("attempt to select more than one element")
# 	if ( !is.na(pmatch(i, names(x@featureData))) | length(value) == numFeatures(x) ) {
# 		x@featureData[,i] <- value
# 	} else if ( !is.na(pmatch(i, names(x@pixelData))) | length(value) == numPixels(x) ) {
# 		x@pixelData[,i] <- value
# 	} else {
# 		x@metaData[[i]] <- value
# 	}
# 	x@metaData[["positionArray"]] <- generatePositionArray(x)
# 	validObject(x)
# 	return(x)
# } )

# setReplaceMethod("[[", c("MSImageSet", "numeric", "missing"), function(x, i, j, ..., value) {
# 	if ( length(i) > 1 ) stop("attempt to select more than one element")
# 	x@spectra$spectra[,i] <- value
# 	validObject(x)
# 	return(x)
# } )

#### set $ operator ####

setMethod("$", "MSImageSet", function(x, name) {
	if ( !is.na(pmatch(name, names(x@featureData))) ) {
		x@featureData[,name]
	} else if ( !is.na(pmatch(name, names(x@pixelData))) ) {
		x@pixelData[,name]
	} else if ( !is.na(pmatch(name, names(x@metaData))) ) {
		x@metaData[[name]]
	} else {
		stop("could not find member matching ", substitute(name))
	}
} )

setMethod("$", "MSPeakFrame", function(x, name) {
	return(x@peaks[[name]])
} )

#### set $<- operator ####

# setReplaceMethod("$", "MSImageSet", function(x, name, value) {
# 	if ( !is.na(pmatch(name, names(x@featureData))) | length(value) == numFeatures(x) ) {
# 		x@featureData[,name] <- value
# 	} else if ( !is.na(pmatch(name, names(x@pixelData))) | length(value) == numPixels(x) ) {
# 		x@pixelData[,name] <- value
# 	} else {
# 		x@metaData[[name]] <- value
# 	}
# 	x@metaData[["positionArray"]] <- generatePositionArray(x)
# 	validObject(x)
# 	return(x)
# } )

#### helper function for [] subsetting of MSImageSet ####

coordFromCall <- function(x, call, missing.i, missing.j, missing.drop) {
	callList <- as.list(call)[-(1:ifelse(missing.i, 2, 3))]
	if ( !missing.drop ) callList <- callList[-length(callList)]
	if ( missing.j ) callList <- c(list(""), callList)
	if ( length(callList) != length(x@metaData[["coordDimNames"]]) ) {
		stop("incorrect number of dimensions")
	}
	coord <- lapply(coord(x), function(d) 1:max(d))
	name <- sapply(callList, is.name)
	callList[name] <- ""
	callList <- lapply(callList, eval)	
	numeric <- sapply(callList, is.numeric)
	logical <- sapply(callList, is.logical)
	if ( sum(numeric) > 0 ) coord[numeric] <- callList[numeric]
	if ( sum(logical) > 0 ) {
		coord[logical] <- lapply(which(logical), function(i) {
			which(rep(callList[[i]], length.out=max(coord(x)[,i])))
		} )
	}
	names(coord) <- x@metaData[["coordDimNames"]]
	return(coord)
}


