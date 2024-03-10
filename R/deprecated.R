
#### Generics ####

# spectra
setGeneric("iData", function(x, i, ...) standardGeneric("iData"))
setGeneric("iData<-", function(x, i, ..., value) standardGeneric("iData<-"))
setMethod("iData", "ANY", function(x, i, ...) {
		.Deprecated("spectra")
		spectra(x, i, ...)
	})
setReplaceMethod("iData", "ANY", function(x, i, ..., value) {
		.Deprecated("spectra")
		spectra(x, i, ...) <- values
		x
	})

# other
setGeneric("coordLabels", function(object) standardGeneric("coordLabels"))
setGeneric("coordLabels<-", function(object, value) standardGeneric("coordLabels<-"))
setGeneric("peakData", function(object, ...) standardGeneric("peakData"))
setGeneric("peakData<-", function(object, ..., value) standardGeneric("peakData<-"))
setGeneric("mzData", function(object, ...) standardGeneric("mzData"))
setGeneric("mzData<-", function(object, ..., value) standardGeneric("mzData<-"))
setGeneric("intensityData", function(object, ...) standardGeneric("intensityData"))
setGeneric("intensityData<-", function(object, ..., value) standardGeneric("intensityData<-"))

# models and results
setGeneric("modelData", function(object, ...) standardGeneric("modelData"))
setGeneric("modelData<-", function(object, ..., value) standardGeneric("modelData<-"))
setGeneric("resultData", function(object, i, ...) standardGeneric("resultData"))
setGeneric("resultData<-", function(object, i, ..., value) standardGeneric("resultData<-"))
setGeneric("resultNames", function(object, ...) standardGeneric("resultNames"))
setGeneric("resultNames<-", function(object, ..., value) standardGeneric("resultNames<-"))

# resolution
setGeneric("resolution", function(object) standardGeneric("resolution"))
setGeneric("resolution<-", function(object, value) standardGeneric("resolution<-"))

# 'sp' package
setGeneric("gridded", function(obj) standardGeneric("gridded"))
setGeneric("gridded<-", function(obj, value) standardGeneric("gridded<-"))
setGeneric("coordinates", function(obj, ...) standardGeneric("coordinates"))
setGeneric("coordinates<-", function(object, value) standardGeneric("coordinates<-"))
setGeneric("coordnames", function(x) standardGeneric("coordnames"))
setGeneric("coordnames<-", function(x, value) standardGeneric("coordnames<-"))

# imzML metadata
setGeneric("msiInfo", function(object, ...) standardGeneric("msiInfo"))
setGeneric("matrixApplication", function(object) standardGeneric("matrixApplication"))
setGeneric("pixelSize", function(object) standardGeneric("pixelSize"))
setGeneric("instrumentVendor", function(object) standardGeneric("instrumentVendor"))
setGeneric("massAnalyzerType", function(object) standardGeneric("massAnalyzerType"))
setGeneric("ionizationType", function(object) standardGeneric("ionizationType"))
setGeneric("scanPolarity", function(object) standardGeneric("scanPolarity"))
setGeneric("scanType", function(object) standardGeneric("scanType"))
setGeneric("scanPattern", function(object) standardGeneric("scanPattern"))
setGeneric("scanDirection", function(object) standardGeneric("scanDirection"))
setGeneric("lineScanDirection", function(object) standardGeneric("lineScanDirection"))

# preprocessing
setGeneric("normalization", function(object) standardGeneric("normalization"))
setGeneric("normalization<-", function(object, value) standardGeneric("normalization<-"))
setGeneric("smoothing", function(object) standardGeneric("smoothing"))
setGeneric("smoothing<-", function(object, value) standardGeneric("smoothing<-"))
setGeneric("baselineReduction", function(object) standardGeneric("baselineReduction"))
setGeneric("baselineReduction<-", function(object, value) standardGeneric("baselineReduction<-"))
setGeneric("spectrumRepresentation", function(object) standardGeneric("spectrumRepresentation"))
setGeneric("spectrumRepresentation<-", function(object, value) standardGeneric("spectrumRepresentation<-"))
setGeneric("peakPicking", function(object) standardGeneric("peakPicking"))
setGeneric("peakPicking<-", function(object, value) standardGeneric("peakPicking<-"))

setGeneric("smoothSignal", function(object, ...) standardGeneric("smoothSignal"))
setGeneric("mzAlign", function(object, ref, ...) standardGeneric("mzAlign"))
setGeneric("mzBin", function(object, ref, ...) standardGeneric("mzBin"))
setGeneric("mzFilter", function(object, ...) standardGeneric("mzFilter"))
setGeneric("peakBin", function(object, ref, ...) standardGeneric("peakBin"))

# apply
setGeneric("cvApply", function(.x, .y, .fun, ...) standardGeneric("cvApply"))
setGeneric("featureApply", function(.object, .fun, ...) standardGeneric("featureApply"))
setGeneric("pixelApply", function(.object, .fun, ...) standardGeneric("pixelApply"))
setGeneric("spatialApply", function(.object, .r, .fun, ...) standardGeneric("spatialApply"))

# images
setGeneric("height", function(x) standardGeneric("height"))
setGeneric("height<-", function(x, ..., value) standardGeneric("height<-"))

# IO
setGeneric("writeAnalyze", function(object, ...) standardGeneric("writeAnalyze"))

#### Create a 3D image ####

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
