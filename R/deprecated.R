
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
	function(object, ref, tolerance = NA, units = c("ppm", "mz"),
		span = 0.75, control = loess.control(), ...)
	{
		.Deprecated("recalibrate")
		recalibrate(object, ref=ref, tolerance=tolerance, units=units, ...)
	})

setMethod("mzAlign", c("MSImagingExperiment", "missing"),
	function(object, tolerance = NA, units = c("ppm", "mz"),
		span = 0.75, control = loess.control(), quantile = 0.2, ...)
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
			object <- object[fData(object)[["freq"]] >= freq.min],]
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
		.Deprecated("chunkApply")
		.checkForIncompleteProcessing(.object)
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
		.Deprecated("chunkApply")
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

## Preset color maps
color.map <- function(map = c("redblack", "greenblack", "blueblack",
	"viridis", "cividis", "magma", "inferno", "plasma",
	"rainbow", "darkrainbow", "grayscale",
	"jet", "hot", "cool"), n = 100)
{
	.Defunct("matter::cpal")
	map <- match.arg(map)
	switch(map,
		redblack = gradient.colors(n, end="#EE2200"),
		greenblack = gradient.colors(n, end="#00FF44"),
		blueblack = gradient.colors(n, end="#00AAFF"),
		viridis = viridis(n),
		cividis = cividis(n),
		magma = magma(n),
		inferno = inferno(n),
		plasma = plasma(n),
		darkrainbow = intensity.colors(n),
		rainbow = intensity.colors2(n),
		grayscale = bw.colors(n),
		jet = jet.colors(n),
		hot = c(
			divergent.colors(ceiling(n/2), "black", "darkred", "red"),
			divergent.colors(floor(n/2), "red", "orange", "yellow")),
		cool = gradient.colors(n, "cyan", "magenta"))
}

col.map <- function(...) color.map(...)

## Colors for image intensities
intensity.colors <- function(n = 100, alpha = 1) {
	.Defunct("matter::cpal")
	col2 <- rainbow(3*n, alpha=alpha)[(2*n):1]
	f <- colorRamp(c("black", rainbow(3*n)[2*n]))
	alpha <- col2rgb(col2, alpha=TRUE)[[4]]
	col1 <- sapply(seq(from=0, to=1, length.out=n), function(i) do.call("rgb",
		c(as.list(f(i)), maxColorValue=255, alpha=alpha)))
	cols <- c(col1, col2)
	cols[seq(from=1, to=3*n, by=3)]
}

## Colors for image intensities (alternative)
intensity.colors2 <- function(n = 100, alpha = 1) {
	.Defunct("matter::cpal")
	cols <- rainbow(3*n, alpha=alpha)[(2*n):1]
	cols[round(seq(from=1, to=2*n, length.out=n))]
}

## Colors for the "jet" color scheme
jet.colors <- function(n = 100, alpha = 1) {
	.Defunct("matter::cpal")
	col2 <- rainbow(9*n, alpha=alpha)[1:(6*n)]
	f <- colorRamp(c("darkred", rainbow(n)[1]))
	g <- colorRamp(c(col2[length(col2)], "darkblue"))
	alpha <- col2rgb(col2, alpha=TRUE)[[4]]
	col1 <- sapply(seq(from=0, to=1, length.out=n), function(i) do.call("rgb",
		c(as.list(f(i)), maxColorValue=255, alpha=alpha)))
	col3 <- sapply(seq(from=0, to=1, length.out=n), function(i) do.call("rgb",
		c(as.list(g(i)), maxColorValue=255, alpha=alpha)))
	cols <- rev(c(col1, col2, col3))
	cols[seq(from=1, to=8*n, by=8)]
}


## Colors for diverging conditions
divergent.colors <- function(n = 100, start = "#00AAEE",
	middle = "#FFFFFF", end = "#EE2200", alpha = 1)
{
	.Defunct("matter::cpal")
	alpha <- round(alpha * 255)
	f1 <- colorRamp(c(start, middle))
	f2 <- colorRamp(c(middle, end))
	col1 <- sapply(seq(from=0, to=1, length.out=n), function(i) do.call("rgb",
			c(as.list(f1(i)), maxColorValue=255, alpha=alpha)))
	col2 <- sapply(seq(from=0, to=1, length.out=n), function(i) do.call("rgb",
			c(as.list(f2(i)), maxColorValue=255, alpha=alpha)))
	cols <- c(col1, col2)
	cols[seq(from=1, to=2*n, by=2)]
}

## Colors for risk ranging blue to red through white
risk.colors <- function(n = 100, alpha = 1)
{
	.Defunct("matter::cpal")
	cols <- divergent.colors(n=n, start="#0000FF", middle="#FFFFFF", end="#FF0000", alpha=1)
	cols
}

## Gradient of two colors
gradient.colors <- function(n = 100, start = "#000000", end = "#00AAFF", alpha = 1) {
	.Defunct("matter::cpal")
	alpha <- round(alpha * 255)
	f <- colorRamp(c(start, end))
	cols <- sapply(seq(from=0, to=1, length.out=n), function(i) do.call("rgb",
			c(as.list(f(i)), maxColorValue=255, alpha=alpha)))
	cols
}

## Gradient of black and white
bw.colors <- function(n = 100, alpha = 1) {
	.Defunct("matter::cpal")
	cols <- gradient.colors(n=n, start="#000000", end="#FFFFFF", alpha=alpha)
	cols
}

## Discrete color scale
discrete.colors <- function(n = 2, chroma = 150, luminance = 65, alpha = 1) {
	.Defunct("matter::dpal")
	if ( n == 1L )
		return(rgb(0, 4/9, 2/3, alpha))
	hue <- c(0, 360) + 15
	if ( diff(hue) %% 360 < 1 )
		hue[2] <- hue[2] - 360 / n
	rotate <- function(x) x %% 360
	hues <- rotate(seq(hue[1], hue[2], length.out = n))
	cols <- hcl(hues, c=chroma, l=luminance, alpha=alpha)
	cols
}

## Convert a color or vector of colors to be translucent
alpha.colors <- function(col, n = 100, alpha = (seq_len(n)/n)^alpha.power, alpha.power = 2) {
	.Defunct("matter::add_alpha")
	if ( missing(n) )
		n <- length(col)
	if ( length(col) != n )
		col <- rep(col, length.out=n)
	if ( length(alpha) != n )
		alpha <- rep(alpha, length.out=n)
	cols <- col2rgb(col, alpha=TRUE)
	alphas <- 255 * alpha
	cols <- rgb(cols[1,], cols[2,], cols[3,],
		alpha=as.integer(alphas),
		maxColorValue=255)
	cols
}
