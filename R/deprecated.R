
#### Deprecated and defunct ####
## -----------------------------

setMethod("smoothSignal", "SpectralImagingExperiment",
	function(object, method = c("gaussian", "sgolay", "ma"), ...)
	{
		.Deprecated("smooth")
		smooth(object, method=match.arg(method), ...)
	})

setMethod("mzBin", c("MSImagingExperiment", "numeric"),
	function(object, ref, tolerance = NA, units = c("ppm", "mz"), fun="sum", ...)
	{
		.Deprecated("bin")
		bin(object, ref=ref, tolerance=tolerance, units=units, method=fun, ...)
	})

setMethod("mzBin", c("MSImagingExperiment", "missing"),
	function(object, from=min(mz(object)), to=max(mz(object)), by = resolution,
			resolution = NA, units = c("ppm", "mz"), fun="sum", ...)
	{
		.Deprecated("bin")
		bin(object, tolerance=0.5 * resolution, units=units, method=fun, ...)
	})

setMethod("mzAlign", c("MSImagingExperiment", "numeric"),
	function(object, ref, tolerance = NA, units = c("ppm", "mz"), ...)
	{
		.Deprecated("recalibrate")
		recalibrate(object, ref=ref, tolerance=tolerance, units=units, ...)
	})

setMethod("mzAlign", c("MSImagingExperiment", "missing"),
	function(object, tolerance = NA, units = c("ppm", "mz"), ...)
	{
		.Deprecated("recalibrate")
		ref <- rowStats(object, stat="mean")
		ref <- mz(object)[findpeaks(ref, relheight=0)]
		recalibrate(object, ref=ref, tolerance=tolerance, units=units, ...)
	})

setMethod("mzFilter", "MSImagingExperiment",
	function(object, ..., freq.min = NA, rm.zero = TRUE)
	{
		.Deprecated("subsetFeatures")
		if ( !is.na(freq.min) && !is.null(fData(object)[["freq"]]) )
			object <- object[fData(object)[["freq"]] >= freq.min,]
		object
	})

setMethod("peakFilter", "MSImagingExperiment",
	function(object, ..., freq.min = 0.01, rm.zero = TRUE)
	{
		.Deprecated("subsetFeatures")
		if ( !is.na(freq.min) && !is.null(fData(object)[["freq"]]) )
			object <- object[fData(object)[["freq"]] >= freq.min,]
		object
	})

getCardinalNumBlocks <- function() {
	.Deprecated("getCardinalNChunks")
	getCardinalNChunks()
}

setCardinalNumBlocks <- function(n = 20L) {
	.Deprecated("setCardinalNChunks")
	setCardinalNChunks(n)
}

setMethod("featureApply", "SpectralImagingExperiment",
	function(.object, .fun, ..., .simplify = TRUE, .outpath = NULL,
		.blocks = getCardinalNumBlocks(), .verbose = getCardinalVerbose(),
		BPPARAM = getCardinalBPPARAM())
	{
		.Deprecated("rowStats")
		chunkApply(spectra(.object), FUN=.fun, MARGIN=1L, ...,
			simplify=.simplify, nchunks=.blocks,
			outpath=.outpath, verbose=.verbose,
			BPPARAM=BPPARAM)
	})

setMethod("pixelApply", "SpectralImagingExperiment",
	function(.object, .fun, ..., .simplify = TRUE, .outpath = NULL,
		.blocks = getCardinalNumBlocks(), .verbose = getCardinalVerbose(),
		BPPARAM = getCardinalBPPARAM())
	{
		.Deprecated("colStats")
		chunkApply(spectra(.object), FUN=.fun, MARGIN=2L, ...,
			simplify=.simplify, nchunks=.blocks,
			outpath=.outpath, verbose=.verbose,
			BPPARAM=BPPARAM)
	})

setMethod("spatialApply", "SpectralImagingExperiment",
	function(.object, .r, .fun, ..., .dist = "chebyshev",
		.simplify = TRUE, .outpath = NULL,
		.blocks = getCardinalNumBlocks(),
		.verbose = getCardinalVerbose(),
		BPPARAM = getCardinalBPPARAM())
	{
		.Defunct("chunkApply")
	})

## Summarize the pixels or features of an imaging dataset

setMethod("aggregate", "SpectralImagingExperiment",
	function(x, by = c("feature", "pixel"), FUN,
		groups = NULL, tform = identity, as = "ImagingExperiment",
		BPPARAM = getCardinalBPPARAM(), ...)
	{
		.Defunct("summarizeFeatures")
		# .checkForIncompleteProcessing(x)
		# by <- match.arg(by)
		# if ( by == "feature" ) {
		# 	len <- ncol(x)
		# 	df <- fData(x)[,integer(),drop=FALSE]
		# } else if ( by == "pixel" ) {
		# 	len <- nrow(x)
		# 	df <- pData(x)[,integer(),drop=FALSE]
		# }
		# if ( !is.null(groups) ) {
		# 	groups <- rep_len(groups, len)
		# 	groups <- as.factor(groups)
		# }
		# as <- match.arg(as, c("ImagingExperiment", "DataFrame"))
		# groupnames <- levels(groups)
		# ngroups <- nlevels(groups)
		# statnames <- c(
		# 	"prod",
		# 	"mean", "sum",
		# 	"sd", "var",
		# 	"min", "max",
		# 	"all", "any",
		# 	"nnzero")
		# streamstats <- is.character(FUN) && all(FUN %in% statnames)
		# if ( streamstats ) {
		# 	fnames <- names(FUN)
		# 	if ( is.null(fnames) ) {
		# 		fnames <- FUN
		# 	} else {
		# 		ind <- which(!nzchar(fnames))
		# 		fnames[ind] <- FUN[ind]
		# 	}
		# } else if ( is.function(FUN) ) {
		# 	fnames <- deparse(substitute(FUN))
		# 	FUN <- list(FUN)
		# } else {
		# 	fnames <- names(FUN)
		# 	if ( is.null(fnames) ) {
		# 		fnames <- paste0("FUN.", seq_along(FUN))
		# 	} else {
		# 		ind <- which(!nzchar(fnames))
		# 		fnames[ind] <- paste0("FUN.", ind)
		# 	}
		# 	FUN <- lapply(FUN, match.fun)
		# }
		# FUNLIST <- setNames(FUN, fnames)
		# if ( is.null(groups) ) {
		# 	cnames <- fnames
		# } else {
		# 	cnames <- paste0(rep(fnames, each=ngroups), ".",
		# 		rep(unlist(groupnames), times=length(fnames)))
		# }
		# if ( streamstats ) {
		# 	y <- .aggregate_stats(x, by=by, STATS=FUNLIST, ...,
		# 		groups=groups, tform=tform, BPPARAM=BPPARAM)
		# } else {
		# 	y <- .aggregate_funs(x, by=by, FUNLIST=FUNLIST, ...,
		# 		groups=groups, tform=tform, BPPARAM=BPPARAM)
		# }
		# y <- do.call(cbind, y)
		# df[cnames] <- y
		# if ( as == "ImagingExperiment" ) {
		# 	if ( is.null(groups) ) {
		# 		mcols <- data.frame(FUN=fnames)
		# 	} else {
		# 		mcols <- expand.grid(group=groupnames, FUN=fnames)
		# 		mcols <- rev(mcols)
		# 	}
		# 	dimnames(y) <- NULL
		# 	if ( by == "pixel" ) {
		# 		fData <- DataFrame(mcols)
		# 		iData <- ImageArrayList(t(y))
		# 		names(iData) <- names(imageData(x))[1]
		# 		ans <- .SparseImagingSummary(
		# 			imageData=iData,
		# 			featureData=fData,
		# 			elementMetadata=df)
		# 	} else if ( by == "feature" ) {
		# 		pData <- PositionDataFrame(
		# 			coord=expand.grid(x=1:nrow(mcols), y=1),
		# 			run=factor(1), mcols)
		# 		iData <- ImageArrayList(y)
		# 		names(iData) <- names(imageData(x))[1]
		# 		ans <- .SparseImagingSummary(
		# 			imageData=iData,
		# 			featureData=df,
		# 			elementMetadata=pData)
		# 	}
		# } else {
		# 	ans <- df
		# }
		# ans
	})

## image
image3d <- function(
	x = seq(0, 1, length.out=dim(values)[1]),
	y = seq(0, 1, length.out=dim(values)[2]),
	z = seq(0, 1, length.out=dim(values)[3]),
	values,
	xlim = range(x),
	ylim = range(y),
	zlim = range(z),
	xlab, ylab, zlab,
	col = heat.colors(12),
	alpha.power = 1,
	alpha = (seq_along(col) / length(col))^alpha.power,
	pch = 15, cex = 1,
	scale = FALSE,
	add = FALSE,
	...)
{
	.Deprecated("matter::vizi")
	if ( missing(values) ) {
		if ( !missing(x) ) {
			if ( is.list(x) ) {
				values <- x$values
				z <- x$z
				y <- x$y
				x <- x$x
			} else {
				if ( is.null(dim(x)) )
					stop("argument must be array-like")
				values <- x
				x <- seq.int(0, 1, length.out = dim(values)[1])
			}
			if ( missing(xlab) )
				xlab <- ""
			if ( missing(ylab) )
				ylab <- ""
			if ( missing(zlab) )
				zlab <- ""
		} else {
			stop("no 'values' array specified")
		}
    } else {
    	if ( missing(xlab) )
    		xlab <- if ( missing(x) ) "" else deparse(substitute(x))
		if ( missing(ylab) )
    		ylab <- if ( missing(y) ) "" else deparse(substitute(y))
		if ( missing(zlab) )
    		zlab <- if ( missing(z) ) "" else deparse(substitute(z))
    }
    if ( !add || is.null(.Cardinal$trans3d) )
    	.Cardinal$trans3d <- persp(xlim, ylim, matrix(zlim, nrow=2, ncol=2),
			xlim=xlim, ylim=ylim, zlim=zlim,
			xlab=xlab, ylab=ylab, zlab=zlab,
			border=NA, shade=NA, col=NA,
			scale=scale, ...)
    if ( !all(is.na(values)) ) {
    	col <- alpha.colors(col, alpha=alpha)
		bins <- cut(values, breaks=seq(
			from=min(values, na.rm=TRUE),
			to=max(values, na.rm=TRUE),
			length.out=length(col)+1),
			include.lowest=TRUE)
		col <- col[bins]
		coord <- expand.grid(x=x, y=y, z=z)
		points(trans3d(coord$x, coord$y, coord$z, .Cardinal$trans3d),
			col=col, pch=pch, cex=cex)
    }
	invisible(.Cardinal$trans3d)
}

points3d <- function(
	x, y, z,
	values,
	xlim = range(x),
	ylim = range(y),
	zlim = range(z),
	xlab, ylab, zlab,
	col = heat.colors(12),
	alpha.power = 1,
	alpha = (seq_along(col) / length(col))^alpha.power,
	pch = 15, cex = 1,
	scale = FALSE,
	add = FALSE,
	...)
{
	.Deprecated("matter::vizi")
	if ( missing(xlab) )
		xlab <- if ( missing(x) ) "" else deparse(substitute(x))
	if ( missing(ylab) )
		ylab <- if ( missing(y) ) "" else deparse(substitute(y))
	if ( missing(zlab) )
		zlab <- if ( missing(z) ) "" else deparse(substitute(z))
    if ( !add || is.null(.Cardinal$trans3d) )
    	.Cardinal$trans3d <- persp(xlim, ylim, matrix(zlim, nrow=2, ncol=2),
			xlim=xlim, ylim=ylim, zlim=zlim,
			xlab=xlab, ylab=ylab, zlab=zlab,
			border=NA, shade=NA, col=NA,
			scale=scale, ...)
	if ( !all(is.na(values)) ) {
		col <- alpha.colors(col, alpha=alpha)
		bins <- cut(values, breaks=seq(
			from=min(values, na.rm=TRUE),
			to=max(values, na.rm=TRUE),
			length.out=length(col)+1),
			include.lowest=TRUE)
		col <- col[bins]
		points(trans3d(x, y, z, .Cardinal$trans3d),
			col=col, pch=pch, cex=cex)
	}
	invisible(.Cardinal$trans3d)
}


# Set to dark mode
darkmode <- function(default = TRUE) {
	.Deprecated("matter::vizi_style")
	matter::vizi_style("dark")
}

# Set to dark mode
lightmode <- function(default = TRUE) {
	.Deprecated("matter::vizi_style")
	matter::vizi_style("light")
}

