
setMethod("coregister",
	signature = c(object = "SpatialShrunkenCentroids", ref = "missing"),
	function(object, ...) {
		ks <- unlist(object$k)
		nclasses <- sapply(object$classes, function(cs) length(unique(cs)))
		regorder <- order(ks, nclasses)
		result <- resultData(object)
		for ( i in which(!duplicated(ks[regorder])) ) {
			j <- regorder[i]
			classes <- result[[j]]$classes
			order <- rep(NA, nlevels(classes))
			order[unique(classes)] <- seq_along(unique(classes))
			order[is.na(order)] <- setdiff(seq_len(nlevels(classes)),
				seq_along(unique(classes)))
			result[[j]] <- .spatialShrunkenCentroids.reclass(result[[j]], order)
		}
		for ( i in which(duplicated(ks[regorder])) ) {
			j <- regorder[i]
			k <- regorder[i-1]
			ref <- .coregisterCenters(result[[j]], result[[k]])$ref
			result[[j]] <- .spatialShrunkenCentroids.reclass(result[[j]], ref)
		}
		resultData(object) <- result
		object
	})

setMethod("coregister",
	signature = c(object = "SpatialKMeans", ref = "missing"),
	function(object, ...) {
		ks <- unlist(object$k)
		regorder <- order(ks)
		result <- resultData(object)
		for ( i in seq_along(regorder[-1]) ) {
			j <- regorder[i+1]
			k <- regorder[i]
			ref <- .coregisterCenters(result[[j]], result[[k]])$ref
			result[[j]] <- .spatialKMeans.reclass(result[[j]], ref)
		}
		resultData(object) <- result
		object
	})

.coregisterCenters <- function(object, ref) {
	extant <- apply(object$centers, 2, function(o) all(is.finite(o)))
	dists <- apply(object$centers, 2, function(o) colSums((o - ref$centers)^2))
	matched <- sapply(seq_len(ncol(dists)), function(j) {
		for ( i in order(dists[,j]) ) {
			if ( all(is.na(dists[i,]))) {
				match <- NA
				next
			} else if ( j == which.min(dists[i,]) ) {
				match <- i
				break
			} else {
				match <- NA
			}
		}
		match
	})
	matched <- data.frame(object=seq_len(ncol(object$centers)), ref=matched)
	unmatched <- setdiff(which(extant), matched$object[!is.na(matched$ref)])
	matched$ref[unmatched] <- setdiff(seq_len(ncol(object$centers)),
		matched$ref)[seq_along(unmatched)]
	matched$ref[is.na(matched$ref)] <- setdiff(seq_len(ncol(object$centers)),
		matched$ref)
	matched
}

# setMethod("coregisterImages", "MSImageSet", function(object, sliceDimNames=metaData(object)[[
# 	"coordDimNames"]][c(1,2)], refCoord, registerCoord=list(), ...)
# {
# 	if ( (length(sliceDimNames)+length(registerCoord)) < length(object@metaData[["coordDimNames"]]) ) {
# 		regMoreCoord <- lapply(coord(object)[!(object@metaData[["coordDimNames"]] %in% sliceDimNames)], function(xyz) 1:max(xyz))
# 		names(regMoreCoord) <- object@metaData[["coordDimNames"]][!(object@metaData[["coordDimNames"]] %in% sliceDimNames)]
# 		regMoreCoord[names(regMoreCoord) %in% names(registerCoord)] <- registerCoord
# 		registerCoord <- regMoreCoord
# 	}
# 	sliceCoord <- do.call(expand.grid, registerCoord)
# 	if ( missing(refCoord) ) refCoord <- sliceCoord[1,]
# 	slicePixels <- apply(sliceCoord, 1, function(xyz) {
# 		xyz <- as.list(xyz)
# 		names(xyz) <- names(registerCoord)
# 		pix <- pixels(object, coord=xyz)
# 		pix[is.finite(pix)]
# 	} )
# 	if ( is.matrix(slicePixels) ) slicePixels <- lapply(1:ncol(slicePixels), function(i) slicePixels[,i])
# 	whichRef <- which(apply(sliceCoord, 1, function(xyz) all(as.numeric(refCoord) == xyz)))
# 	if ( length(whichRef) == 0 ) stop("'refCoord' must appear in 'registerCoord'")
# 	for ( i in (whichRef - 1):1 ) {
# 		if ( i < 1 ) break
# 		tryVerboseMessage("Coregistering ", paste(names(registerCoord), "=",
# 			unlist(sliceCoord[i,]), collapse=", "), " with ",
# 			paste(names(registerCoord), "=", unlist(sliceCoord[i + 1,]),
# 			collapse=", "), precedes.progress.output=FALSE)
# 		par <- findTransformation(object, sliceDimNames, slicePixels[[i + 1]], slicePixels[[i]])
# 		object <- rotateSlice(object, slicePixels[[i]], sliceDimNames, par[1:2], par[3])
# 	}
# 	for ( i in (whichRef + 1):length(slicePixels) ) {
# 		if ( i > length(slicePixels) ) break
# 		tryVerboseMessage("Coregistering ", paste(names(registerCoord), "=",
# 			unlist(sliceCoord[i,]), collapse=", "), " with ",
# 			paste(names(registerCoord), "=", unlist(sliceCoord[i - 1,]),
# 			collapse=", "), precedes.progress.output=FALSE)
# 		par <- findTransformation(object, sliceDimNames, slicePixels[[i - 1]], slicePixels[[i]])
# 		object <- rotateSlice(object, slicePixels[[i]], sliceDimNames, par[1:2], par[3])
# 	}
# 	positionArray <- generatePositionArray(object@pixelData[
# 		object@metaData[["coordDimNames"]]])
# 	kept <- seq_len(numPixels(object)) %in% positionArray
# 	new.spectra <- new.env(parent=globalenv())
# 	new.spectra$spectra <- object@spectra$spectra[,kept,drop=FALSE]
# 	object@spectra <- new.spectra
# 	object@pixelData <- object@pixelData[kept,,drop=FALSE]
# 	if ( sum(!kept) > 0 ) warning(sum(!kept), " pixel(s) dropped")
# 	object@metaData[["history"]][[date()]] <- generateHistory(match.call(call=sys.call(-1)))
# 	object@metaData[["positionArray"]] <- generatePositionArray(object@pixelData[
# 		object@metaData[["coordDimNames"]]])
# 	validObject(object)
# 	return(object)
# } )

# findTransformation <- function(object, sliceDimNames,
# 	pixels.reference, pixels.target)
# {
# 	img1 <- createMSImageSet(spectra=intensities(object, pixel=pixels.reference, drop=TRUE),
# 		mz=mz(object), coord=coord(object)[pixels.reference,sliceDimNames])
# 	img2 <- createMSImageSet(spectra=intensities(pig3d.peaks, pixel=pixels.target, drop=TRUE),
# 		mz=mz(object), coord=coord(object)[pixels.target,sliceDimNames])
# 	s1 <- intensities(img1)
# 	fn <- function(par) {
# 		new.coord <- affine(coord(img2), translate=c(par[1], par[2]), rotate=par[3])
# 		coord(img2) <- new.coord
# 		img2 <- regeneratePositions(img2)
# 		s2 <- intensities(img2)
# 		d2 <- min(dim(s1)[2], dim(s2)[2])
# 		d3 <- min(dim(s1)[3], dim(s2)[3])
# 		-sum((log(s1[,1:d2,1:d3]+1) * log(s2[,1:d2,1:d3]+1))^2, na.rm=TRUE)
# 	}
# 	offset <- sapply(coord(img1)[,sliceDimNames], mean) - sapply(coord(img2)[,sliceDimNames], mean)
# 	trace <- getOption("Cardinal.verbose.output")
# 	optim(par=c(offset[1],offset[2],0), fn=fn,control=list(trace=trace))$par
# }

# rotateSlice <- function(object, pixel, sliceDimNames, translate, rotate, ...)
# {
# 	coord(object)[pixel,sliceDimNames] <- affine(coord(object)[pixel,sliceDimNames],
# 		translate=translate, rotate=rotate, ...)
# 	regeneratePositions(object)
# }
