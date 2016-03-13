
#### Baseline reduction methods ####
## ---------------------------------

setMethod("reduceBaseline", "MSImageSet",
	function(object, method = "median",
		...,
		pixel = pixels(object),
		plot = FALSE)
	{
		if ( centroided(object) )
			.stop("reduceBaseline: Data already centroided. Baseline reduction will not be performed.")
		fun <- reduceBaseline.method(method)
		prochistory(processingData(object)) <- .history()
		.message("reduceBaseline: Using method = ", match.method(method))
		.time.start()
		data <- pixelApply(object, function(s, ...) {
			reduceBaseline.do(s, object, .Index, fun, plot, ...)
		}, .pixel=pixel, ..., .use.names=FALSE, .simplify=TRUE)
		object@imageData <- MSImageData(data=data,
			coord=coord(object)[pixel,],
			storageMode=storageMode(object@imageData),
			dimnames=list(featureNames(object), pixelNames(object)[pixel]))
		object@pixelData <- object@pixelData[pixel,]
		baselineReduction(processingData(object)) <- match.method(method)
		.message("reduceBaseline: Done")
		.time.stop()
		object
	})

reduceBaseline.do <- function(s, object, pixel, fun, plot, ...) {
	sout <- fun(s, ...)
	if ( plot ) {
		wrap(plot(object, s ~ mz, pixel=pixel, col="gray",
			ylab="Intensity", strip=FALSE, ...),
			..., signature=fun)
		wrap(lines(mz(object), s - sout, col="green", ...),
			..., signature=fun)
		wrap(lines(mz(object), sout, lwd=0.5, ...),
			..., signature=fun)
	}
	sout
}

reduceBaseline.method <- function(method, name.only=FALSE) {
	if ( is.character(method) || is.null(method) ) {
		options <- "median"
		method <- match.method(method, options)
		if ( name.only )
			return(method)
		method <- switch(method,
			median = reduceBaseline.median,
			match.fun(method))
	}
	match.fun(method)
}

reduceBaseline.median <- function(x, blocks=500, fun=min, spar=1, ...) {
	xint <- blocks(x, blocks=blocks)
	baseval <- sapply(xint, fun)
	baseidx <- sapply(xint, function(xi) which.min(abs(fun(xi) - xi)))
	baseidx <- baseidx + c(0, cumsum(sapply(xint, length))[-length(xint)])
	if ( diff(range(baseval))==0 )
		return(rep(0, length(x)))
	if ( !is.na(spar) ) {
		cutoff <- smooth.spline(x=baseidx, y=baseval, spar=spar)$y
		keep <- which(baseval <= cutoff)
		baseidx <- baseidx[keep]
		baseval <- baseval[keep]
	}
	baseval[c(1,length(baseval))] <- c(fun(xint[[1]]), fun(xint[[length(xint)]]))
	baseidx[c(1,length(baseidx))] <- c(1, length(x))
	baseline <- interp1(x=baseidx, y=baseval, xi=seq_along(x), method="linear")
	pmax(x - baseline, 0)
}
