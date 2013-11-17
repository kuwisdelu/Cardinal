
#### implement the trellis image methods ####

setMethod("trellisImage", "MSImageSet", function(x, mz, feature, values,
	sliceDimNames=metaData(x)[["coordDimNames"]][c(1,2)], fixCoord=list(),
	xlab=sliceDimNames[[1]], ylab=sliceDimNames[[2]], main=metaData(x)[["name"]],
	xlim=range(coord(x)[sliceDimNames[[1]]]), ylim=range(coord(x)[sliceDimNames[[2]]]),
	col.regions=intensity.colors(100), colorkey=list(col=col.regions),
	at=seq(from=min(values, na.rm=TRUE), to=max(values, na.rm=TRUE),
	length.out=length(col.regions)), contrast=c("none", "suppress", "histeq"),
	smoothing=c("none", "gaussian", "adaptive"), interpolate=c("none", "bilinear"),
	fun=mean, ...)
{
	smoothing <- smoothingFunction(smoothing)
	contrast <- contrastFunction(contrast)
	interpolate <- interpolateFunction(interpolate)
	prep.image <- list()
	prep.values <- list()
	# get the flattened values for each feature(s) or m/z value(s)
	if ( missing(values) ) {
		if ( !missing(mz) ) {
			if ( !is.list(mz) ) mz <- list(mz)
			n <- length(mz)
			for ( i in seq_len(n) ) {
				# get the prepared image for that m/z value(s)
				prep.image[[i]] <- prepareImage(x, mz=mz[[i]],
					sliceDimNames=sliceDimNames, fixCoord=fixCoord, trellis=TRUE)
			}
		} else if ( !missing(feature) ) {
			mz <- NULL # needed to make mz() work
			if ( !is.list(feature) ) feature <- list(feature)
			n <- length(feature)
			for ( i in seq_len(n) ) {
				# get the prepared image for that feature(s)
				prep.image[[i]] <- prepareImage(x, feature=feature[[i]],
					sliceDimNames=sliceDimNames, fixCoord=fixCoord, trellis=TRUE)
				mz[[i]] <- round(range(mz(x)[feature[[i]]]), 2)
				if ( length(feature[[i]]) == 1 ) mz[[i]] <- mz[[i]][[1]]
			}
		} else {
			stop("either 'mz', 'feature', or 'values' must be specified")
		}
	} else {
		if ( !is.list(values) ) values <- list(values)
		n <- length(values)
		for ( i in seq_len(n) ) {
			# get the prepared image for those values)
			prep.image[[i]] <- prepareImage(x, values=values[[i]],
				sliceDimNames=sliceDimNames, fixCoord=fixCoord, trellis=TRUE)
		}
	}
	for ( i in seq_len(n) ) {
		# average over the features (but not the other dimensions)
		prep.values[[i]] <- apply(prep.image[[i]]$values,
			c(prep.image[[i]]$sliceWhichDim + 1, prep.image[[i]]$fixWhichDim + 1), fun)
		if ( length(prep.image[[i]]$fixWhichDim) > 0 ) {
			prep.values[[i]] <- apply(prep.values[[i]], prep.image[[i]]$fixWhichDim, function(x) {
				interpolate(smoothing(contrast(x, ...), ...), ...)
			} )
			dim(prep.values[[i]]) <- sapply(prep.image[[i]]$coord, length)
		} else {
			prep.values[[i]] <- interpolate(smoothing(contrast(prep.values[[i]], ...), ...), ...)
		}
	}
	# re-arrange the array dimensions for plotting
	fixCoord <- prep.image[[1]]$fixCoord
	prep.values <- do.call(abind, list(prep.values, along=length(dim(prep.values[[1]])) + 1))
	if ( length(dim(prep.values)) > 4 ) {
		coordDimSize <- prod(dim(prep.values)) / prod(dim(prep.values)[c(1:2,
			length(dim(prep.values)))])
		dim(prep.values) <- c(dim(prep.values)[c(1,2)], coordDimSize,
			dim(prep.values)[length(dim(prep.values))])
	}
	dimnames(prep.values) <- lapply(dim(prep.values), seq_len)
	# get strip labels
	if ( missing(mz) ) {
		featurelab <- names(values)
		if ( is.null(featurelab) ) {
			if ( is.list(values) ) {
				featurelab <- paste(seq_along(values))
			} else {
				featurelab <- paste("")
			}
		}
	} else {
		featurelab <- sapply(prep.image, function(prep) formatMZ(mz(x)[prep$feature]))
	}
	coord <- expand.grid(fixCoord)
	if ( nrow(coord) > 1 ) {
		coordlab <- apply(coord, 1, formatCoord)
		striplab <- list(coordlab, featurelab)
	} else {
		striplab <- list(featurelab)
	}
	# set up the values and plotting coordinates
	values <- prep.values
	plot.coord.x <- seq(min(coord(x)[sliceDimNames[[1]]]), max(coord(x)[sliceDimNames[[1]]]),
		length.out=dim(values)[1])
	plot.coord.y <- seq(min(coord(x)[sliceDimNames[[2]]]), max(coord(x)[sliceDimNames[[2]]]),
		length.out=dim(values)[2])
	dimnames(values)[[1]] <- plot.coord.x
	dimnames(values)[[2]] <- plot.coord.y
	# finally, make the image
	xlim <- sapply(xlim, function(i) which.min(abs(i - plot.coord.x)))
	ylim <- sapply(ylim, function(i) which.min(abs(i - plot.coord.y)))
	# xlim <- 1+xlim-min(plot.coord.x)
	# ylim <- 1+ylim-min(plot.coord.y)
	levelplot(values, main=main, xlab=xlab, ylab=ylab, col.regions=col.regions,
		colorkey=colorkey, at=at, xlim=xlim, ylim=ylim,
		strip=function(..., which.given, factor.levels) {
			if ( which.given == 1 ) {
				strip.default(which.given=which.given, factor.levels=striplab[[1]], ...)
			} else if ( which.given == 2 ) {
				strip.default(which.given=which.given, factor.levels=striplab[[2]], ...)
			}
		},
		xscale.components = function(...) {
			ans <- xscale.components.default(seq_len(dim(values)[[1]]))
			ans <- xscale.components.default(xlim)
			ans$bottom$labels$labels <- round(plot.coord.x[ans$bottom$labels$at])
			ans
		},
		yscale.components = function(...) {
			ans <- yscale.components.default(seq_len(dim(values)[[2]]))
			ans <- yscale.components.default(ylim)
			ans$left$labels$labels <- round(plot.coord.y[ans$left$labels$at])
			ans
		}, ...)
} )

setMethod("trellisImage", "MSImageSegmentation", function(x, which=metaData(x)[["parnames"]],
	mode=c("classes", "probabilities"),  main=metaData(x)[["name"]], colorkey=FALSE,
	col.regions=rainbow(max(x[["nclasses"]][which])),
	at=seq(0, length(col.regions)) + 0.5,
	key=list(text=list(metaData(x)[["labels"]][1:length(col.regions)]),
	rectangles=list(col=col.regions), columns=min(4, length(col.regions))),
	true.labels=FALSE, mask.missing=FALSE, class.mask=NULL, ...)
{
	mode <- match.arg(mode)
	which <- match.which(x, which)
	plot.obj <- createMSImageSet(matrix(0, nrow=1, ncol=nrow(x@metaData$coord)),
		mz=1, coord=x@metaData$coord)
	if ( mode == "classes" || true.labels ) {
		if ( true.labels && !is.null(x$labels) ) {
			values <- list(x$labels)
		} else {
			values <- x[[mode]][which]
		}
		if ( mask.missing && !is.null(x$labels) ) {
			for ( i in seq_along(values) ) values[[i]][is.na(x$labels)] <- NA
		}
		if ( !is.null(class.mask) ) {
			for ( i in seq_along(values) ) values[[i]][!values[[i]] %in% class.mask] <- NA
		}
		names(values) <- metaData(x)[["parnames"]][which]
		trellisImage(plot.obj, values=values, main=main, col.regions=col.regions,
			colorkey=colorkey, at=at, key=key, ...)
	} else if ( mode == "probabilities" ) {
		values <- x[[mode]][which]
		nlayers <- max(sapply(values, ncol))
		if ( mask.missing && !is.null(x$labels) ) {
			for ( i in seq_along(values) ) values[[i]][is.na(x$labels),] <- NA
		}
		for ( i in seq_along(values) ) {
			if ( nlayers > ncol(values[[i]]) ) {
				na <- matrix(NA, ncol=nlayers - ncol(values[[i]]), nrow=nrow(values[[i]]))
				values[[i]] <- cbind(values[[i]], na)
			}
		}
		pcol <- col2rgb(col.regions, alpha=TRUE)
		pcol <- apply(pcol, 2, function(cl) rgb(cl[1], cl[2], cl[3], alpha=1:255, max=255))
		if ( is.null(class.mask) ) class.mask <- 1:nlayers
		names(values) <- metaData(x)[["parnames"]][which]
		trellis.obs <- lapply(class.mask, function(i) {
			probs <- lapply(values, function(p) p[,i])
			trellisImage(plot.obj, values=probs, main=main, col.regions=pcol[,i],
				colorkey=colorkey, key=key, ...)
		} )
		eval(parse(text=paste(paste("trellis.obs[[", 1:length(trellis.obs), "]]", sep=""), collapse="+")))
	}
} )


