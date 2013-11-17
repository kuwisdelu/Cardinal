
#### implement the image methods ####

setMethod("image", "MSImageSet", function(x, mz, feature, values,
	sliceDimNames=metaData(x)[["coordDimNames"]][c(1,2)], fixCoord=list(),
	xlab=sliceDimNames[[1]], ylab=sliceDimNames[[2]], col=intensity.colors(100),
	sub=formatMZ(mz), col.sub="gray", main=metaData(x)[["name"]], fun=mean,
	contrast=c("none", "suppress", "histeq"), smoothing=c("none", "gaussian", "adaptive"),
	interpolate=c("none", "bilinear"), xlim=range(coord(x)[sliceDimNames[[1]]]),
	ylim=range(coord(x)[sliceDimNames[[2]]]), zlim=NULL, colorkey=list(col=col),
	auto.scale=c("variable", "max"), asp=1, add=FALSE, ...)
{
	# get the prepare image
	prep.image <- prepareImage(x, mz, feature, values, sliceDimNames, fixCoord)
	# get m/z values printing subtitle
	if ( missing(mz) || missing(feature) ) mz <- Cardinal:::mz(x)[prep.image$feature]
	# average over all other dimensions but the sliced ones
	values <- apply(prep.image$values, prep.image$sliceWhichDim + 1, fun)
	# get the maximum value
	auto.scale <- match.arg(auto.scale)
	if ( auto.scale == "max" ) int.max <- max(values, na.rm=TRUE)
	# apply processing to the image
	smoothing <- smoothingFunction(smoothing)
	contrast <- contrastFunction(contrast)
	interpolate <- interpolateFunction(interpolate)
	values <- interpolate(smoothing(contrast(values, ...), ...), ...)
	if ( auto.scale == "max" ) values <- int.max * values / max(values, na.rm=TRUE)
	# set up plotting coordinates
	plot.coord.x <- seq(min(coord(x)[sliceDimNames[[1]]]),
		max(coord(x)[sliceDimNames[[1]]]), length.out=nrow(values))
	plot.coord.y <- seq(min(coord(x)[sliceDimNames[[2]]]),
		max(coord(x)[sliceDimNames[[2]]]), length.out=ncol(values))
	# hack to make it easy to specify default zlim regardless of processing
	if ( is.null(zlim) ) zlim <- range(values, na.rm=TRUE)
	# finally, make the image
	suppressWarnings(image(x=plot.coord.x, y=plot.coord.y, z=values, main=main,
		xlab=xlab, ylab=ylab, col=col, sub=sub, col.sub=col.sub,
		xlim=xlim, ylim=ylim, zlim=zlim, asp=asp, add=add, ...))
	if ( isTRUE(colorkey) ) colorkey <- list(col=col)
	if ( is.list(colorkey) ) {
		legend("topright", legend=c(round(zlim[[2]], 2), rep(NA, length(colorkey$col)-2), round(zlim[[1]], 2)),
			col=rev(colorkey$col), lwd=2, y.intersp=0.1, bg=rgb(1, 1, 1, 0.75), cex=0.6)
	}
	invisible(NULL)
} )

setMethod("image", "MSImageSegmentation", function(x, which=1,
	mode=c("classes", "probabilities"), main=metaData(x)[["name"]],
	col=rainbow(max(x[["nclasses"]][which])),
	breaks=seq(0, length(col)) + 0.5, sub=metaData(x)[["parnames"]][[which]],
	true.labels=FALSE, mask.missing=FALSE, class.mask=NULL,
	legend=TRUE, add=FALSE, ...)
{
	mode <- match.arg(mode)
	which <- match.which(x, which)
	plot.obj <- createMSImageSet(matrix(0, nrow=1, ncol=nrow(x@metaData$coord)),
		mz=1, coord=x@metaData$coord)
	if ( mode == "classes" || true.labels ) {
		if ( true.labels && !is.null(x$labels) ) {
			values <- x$labels
		} else {
			values <- x[[mode]][[which]]
		}
		if ( mask.missing && !is.null(x$labels) ) {
			values[is.na(x$labels)] <- NA
		}
		if ( !is.null(class.mask) ) {
			values[!values %in% class.mask] <- NA
		}
		image(plot.obj, values=values, main=main, colorkey=FALSE, col=col, breaks=breaks, sub=sub, add=add, ...)
	} else if ( mode == "probabilities" ) {
		values <- x[[mode]][[which]]
		if ( mask.missing && !is.null(x$labels) ) {
			values[is.na(x$labels),] <- NA
		}
		pcol <- col2rgb(col, alpha=TRUE)
		pcol <- apply(pcol, 2, function(cl) rgb(cl[1], cl[2], cl[3], alpha=1:255, max=255))
		image(plot.obj, values=rep(1, nrow(values)), main=main, colorkey=FALSE,
			zlim=c(-1, 0), sub=sub, add=add, ...)
		if ( is.null(class.mask) ) class.mask <- 1:ncol(values)
		for( i in class.mask ) {
			image(plot.obj, values=values[,i], zlim=c(0,1), col=pcol[,i],
				colorkey=FALSE, add=TRUE, ...)
		}
	}
	if ( legend ) {
		legend("topright", legend=x@metaData$labels[1:length(col)], fill=col,
			bg=rgb(1, 1, 1, 0.75))
	}
	invisible(NULL)
} )

#### helper functions ####

prepareImage <- function(x, mz, feature, values, sliceDimNames, fixCoord, trellis=FALSE) {
	if ( missing(values) && missing(feature) ) {
		if ( missing(mz) ) {
			stop("either 'mz', 'feature', or 'values' must be specified")
		} else if ( length(mz) > 2 ) {
			stop("invalid mz value or mz limits")
		}
		feature <- features(x, mz[[1]]):features(x, mz[[length(mz)]])
	}
	# create coordinates from sliceDimNames
	if ( length(sliceDimNames) != 2 | !is.character(sliceDimNames) ) {
		stop("'sliceDimNames' must list the name of 2 dimensions in 'coordDimNames'")	
	} else {
		mins <- lapply(coord(x)[,sliceDimNames], min)
		maxs <- lapply(coord(x)[,sliceDimNames], max)
		coord <- list(mins[[1]]:maxs[[1]], mins[[2]]:maxs[[2]])
		names(coord) <- sliceDimNames
	}
	# make sure this works even if names are in an unexpected order
	whichDim <- seq_along(x@metaData[["coordDimNames"]])
	names(whichDim) <- x@metaData[["coordDimNames"]]
	sliceWhichDim <- whichDim[sliceDimNames]
	fixWhichDim <- whichDim[-sliceWhichDim]
	# if fixCoord is not long enough, set up the remaining dimenions
	if ( (length(sliceDimNames)+length(fixCoord)) < length(x@metaData[["coordDimNames"]]) ) {
		if ( trellis ) {
			fixMoreCoord <- lapply(coord(x)[!(names(whichDim) %in% sliceDimNames)], function(xyz) 1:max(xyz))
		} else {
			fixMoreCoord <- rep(list(1), length(whichDim) - length(sliceWhichDim))
		}
		names(fixMoreCoord) <- names(whichDim)[!(names(whichDim) %in% sliceDimNames)]
		fixMoreCoord[names(fixMoreCoord) %in% names(fixCoord)] <- fixCoord
		fixCoord <- fixMoreCoord
	}
	if ( all(sapply(fixCoord, length) == 1) ) fixWhichDim <- NULL
	coord <- c(coord, fixCoord)
	# get intensities to plot, or whatever other values we plot instead
	if ( missing(values) ) {
		values <- intensities(x, feature=feature, coord=coord, drop=FALSE)
	} else {
		feature <- NULL
		if ( length(values) %% numPixels(x) != 0 ) {
			stop("length of 'values' is not a multiple of the number of pixels")
		}
		pixels <- getPixels(x, coord=coord, drop=FALSE)
		values <- values[pixels]
		dim(values) <- c(1, dim(pixels))
	}
	list(values=values, feature=feature, coord=coord, fixCoord=fixCoord,
		sliceWhichDim=sliceWhichDim, fixWhichDim=fixWhichDim)
}
