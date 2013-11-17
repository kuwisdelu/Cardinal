
#### implement the volume methods ####

setMethod("volume", "MSImageSet", function(x, mz, feature, values,
	volDimNames=metaData(x)[["coordDimNames"]][c(1,2,3)], fixCoord=list(),
	xlab=volDimNames[[1]], ylab=volDimNames[[2]], zlab=volDimNames[[3]],
	col=alpha.colors(col=intensity.colors(100), ramp="linear", range=c(0,1)),
	sub=formatMZ(mz), col.sub="gray", main=metaData(x)[["name"]], fun=mean,
	contrast=c("none", "suppress", "histeq"), smoothing=c("none", "gaussian", "adaptive"),
	xlim=range(coord(x)[volDimNames[[1]]]), ylim=range(coord(x)[volDimNames[[2]]]),
	zlim=range(coord(x)[volDimNames[[3]]]), tlim=NULL, colorkey=list(col=col),
	auto.scale=c("variable", "max"), pch=20, cex=0.2, scale=FALSE,
	theta=-35, phi=45, add=FALSE, ...)
{
	# get the prepare image
	prep.image <- prepareVolume(x, mz, feature, values, volDimNames, fixCoord)
	# get m/z values printing subtitle
	if ( missing(mz) || missing(feature) ) mz <- Cardinal:::mz(x)[prep.image$feature]
	# average over all other dimensions but the sliced ones
	values <- apply(prep.image$values, prep.image$volWhichDim + 1, fun)
	# get the maximum value
	auto.scale <- match.arg(auto.scale)
	if ( auto.scale == "max" ) int.max <- max(values, na.rm=TRUE)
	# apply processing to the image
	smoothing <- smoothingFunction(smoothing)
	contrast <- contrastFunction(contrast)
	values <- smoothing(contrast(values, ...), ...)
	if ( auto.scale == "max" ) values <- int.max * values / max(values, na.rm=TRUE)
	# set up plotting coordinates
	plot.coord.x <- seq(min(coord(x)[volDimNames[[1]]]),
		max(coord(x)[volDimNames[[1]]]), length.out=dim(values)[[1]])
	plot.coord.y <- seq(min(coord(x)[volDimNames[[2]]]),
		max(coord(x)[volDimNames[[2]]]), length.out=dim(values)[[2]])
	plot.coord.z <- seq(min(coord(x)[volDimNames[[3]]]),
		max(coord(x)[volDimNames[[3]]]), length.out=dim(values)[[3]])
	plot.coord <- expand.grid(x=plot.coord.x, y=plot.coord.y, z=plot.coord.z)
	# hack to make it easy to specify default zlim regardless of processing
	if ( is.null(tlim) ) tlim <- range(values, na.rm=TRUE)
	# prepare the colors
	which.na <- is.na(values)
	values[which.na] <- min(values, na.rm=TRUE)
	cuts <- cut(values, breaks=seq(from=min(values), to=max(values),
		length.out=length(col)), include.lowest=TRUE)
	colors <- col[cuts]
	values[which.na] <- NA
	# prepare plotting region
	if ( !add ) {
		.cardinalState$rot3D <- suppressWarnings(persp(0:1, 0:1,
			matrix(0, nrow=2, ncol=2), xlim=xlim, ylim=ylim, zlim=zlim,
			border=NA, shade=NA, xlab=xlab, ylab=ylab, zlab=zlab,
			scale=scale, theta=theta, phi=phi, main=main,
			sub=sub, col.sub=col.sub, ...))
	}
	# finally, make the image
	suppressWarnings(points(trans3d(plot.coord$x, plot.coord$y, plot.coord$z,
		.cardinalState$rot3D), col=colors, pch=pch, cex=cex, ...))
	if ( isTRUE(colorkey) ) colorkey <- list(col=col)
	if ( is.list(colorkey) ) {
		legend("topright", legend=c(round(zlim[[2]], 2), rep(NA, length(colorkey$col)-2), round(zlim[[1]], 2)),
			col=rev(colorkey$col), lwd=2, y.intersp=0.1, bg=rgb(1, 1, 1, 0.75), cex=0.6)
	}
	invisible(.cardinalState$rot3D)
} )

#### helper functions ####

prepareVolume <- function(x, mz, feature, values, volDimNames, fixCoord, trellis=FALSE) {
	if ( missing(values) && missing(feature) ) {
		if ( missing(mz) ) {
			stop("either 'mz', 'feature', or 'values' must be specified")
		} else if ( length(mz) > 2 ) {
			stop("invalid mz value or mz limits")
		}
		feature <- features(x, mz[[1]]):features(x, mz[[length(mz)]])
	}
	# create coordinates from sliceDimNames
	if ( length(volDimNames) != 3 | !is.character(volDimNames) ) {
		stop("'volDimNames' must list the name of 3 dimensions in 'coordDimNames'")	
	} else {
		mins <- lapply(coord(x)[,volDimNames], min)
		maxs <- lapply(coord(x)[,volDimNames], max)
		coord <- list(mins[[1]]:maxs[[1]], mins[[2]]:maxs[[2]], mins[[3]]:maxs[[3]])
		names(coord) <- volDimNames
	}
	# make sure this works even if names are in an unexpected order
	whichDim <- seq_along(x@metaData[["coordDimNames"]])
	names(whichDim) <- x@metaData[["coordDimNames"]]
	volWhichDim <- whichDim[volDimNames]
	fixWhichDim <- whichDim[-volWhichDim]
	# if fixCoord is not long enough, set up the remaining dimenions
	if ( (length(volDimNames)+length(fixCoord)) < length(x@metaData[["coordDimNames"]]) ) {
		if ( trellis ) {
			fixMoreCoord <- lapply(coord(x)[!(names(whichDim) %in% volDimNames)], function(xyz) 1:max(xyz))
		} else {
			fixMoreCoord <- rep(list(1), length(whichDim) - length(volWhichDim))
		}
		names(fixMoreCoord) <- names(whichDim)[!(names(whichDim) %in% volDimNames)]
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
		volWhichDim=volWhichDim, fixWhichDim=fixWhichDim)
}

